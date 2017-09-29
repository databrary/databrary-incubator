{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes, RecordWildCards, DataKinds #-}
module Databrary.Model.Container
  ( module Databrary.Model.Container.Types
  , blankContainer
  , lookupContainer
  , lookupVolumeContainer
  , lookupVolumeContainers
  , lookupVolumeTopContainer
  , containerIsVolumeTop
  , addContainer
  , changeContainer
  , removeContainer
  , getContainerDate
  , formatContainerDate
  , containerRowJSON
  , containerJSON
  ) where

import Control.Monad (guard)
import Data.Either (isRight)
import Data.Monoid ((<>))
import Data.Time.Format (formatTime, defaultTimeLocale)
import Database.PostgreSQL.Typed.Query (pgSQL)
import Database.PostgreSQL.Typed.Query (makePGQuery, QueryFlags(..), simpleQueryFlags)

import Databrary.Ops
import Databrary.Has (view, peek)
import Databrary.Service.DB
import qualified Databrary.JSON as JSON
import Databrary.Model.SQL (selectQuery, isForeignKeyViolation)
import Databrary.Model.Time
import Databrary.Model.Permission
import Databrary.Model.Id.Types
import Databrary.Model.Party.Types
import Databrary.Model.Identity
import Databrary.Model.Audit
import Databrary.Model.Volume.Types
import Databrary.Model.Volume.SQL (makeVolume, setCreation)
import Databrary.Model.Container.Types
import Databrary.Model.Container.SQL

$(useTDB)

blankContainer :: Volume -> Container
blankContainer vol = Container
  { containerRow = ContainerRow
    { containerId = error "blankContainer"
    , containerTop = False
    , containerName = Nothing
    , containerDate = Nothing
    }
  , containerRelease = Nothing
  , containerVolume = vol
  }

lookupContainer :: (MonadDB c m, MonadHasIdentity c m) => Id Container -> m (Maybe Container)
lookupContainer ci = do
  ident <- peek
  mRow <- dbQuery1
      $(makePGQuery
          (simpleQueryFlags { flagPrepare = Just [] })
          (   "SELECT container.id,container.top,container.name,container.date,slot_release.release,volume.id,volume.name,volume.body,volume.alias,volume.doi,volume_creation(volume.id),volume_owners.owners,volume_permission.permission"
           ++ " FROM container LEFT JOIN slot_release ON container.id = slot_release.container AND slot_release.segment = '(,)' JOIN volume LEFT JOIN volume_owners ON volume.id = volume_owners.volume JOIN LATERAL (VALUES (CASE WHEN ${identitySuperuser ident} THEN enum_last(NULL::permission) ELSE volume_access_check(volume.id, ${view ident :: Id Party}) END)) AS volume_permission (permission) ON volume_permission.permission >= 'PUBLIC'::permission ON container.volume = volume.id "
           ++ "WHERE container.id = ${ci}" ))
  pure 
    (fmap 
       (\(cid,ctp,cnm,cdt,srl,vid2,vnm,vbd,vals,vdoi,vcr,vow,vpr) -> 
          ($)
            (Container (ContainerRow cid ctp cnm cdt) srl)
            (makeVolume
              (setCreation (VolumeRow vid2 vnm vbd vals vdoi) vcr)
              vow
              vpr))
       mRow)

lookupVolumeContainer :: MonadDB c m => Volume -> Id Container -> m (Maybe Container)
lookupVolumeContainer vol ci = do
  mRow <- dbQuery1
      $(makePGQuery
          (simpleQueryFlags { flagPrepare = Just [] })
          (   "SELECT container.id,container.top,container.name,container.date,slot_release.release"
           ++ " FROM container LEFT JOIN slot_release ON container.id = slot_release.container AND slot_release.segment = '(,)'"
           ++ " WHERE container.id = ${ci} AND container.volume = ${volumeId $ volumeRow vol}" ))
  pure 
    (fmap 
       (\(vid, tp, nm, dt, rl) -> 
          Container (ContainerRow vid tp nm dt) rl vol)
       mRow)

lookupVolumeContainers :: MonadDB c m => Volume -> m [Container]
lookupVolumeContainers vol = do
  rows <- dbQuery
      $(makePGQuery
          (simpleQueryFlags { flagPrepare = Just [] })
          (   "SELECT container.id,container.top,container.name,container.date,slot_release.release"
           ++ " FROM container LEFT JOIN slot_release ON container.id = slot_release.container AND slot_release.segment = '(,)'"
           ++ " WHERE container.volume = ${volumeId $ volumeRow vol} ORDER BY container.id" ))
  pure 
    (fmap 
       (\(vid, tp, nm, dt, rl) -> 
          Container (ContainerRow vid tp nm dt) rl vol)
       rows)

lookupVolumeTopContainer :: MonadDB c m => Volume -> m Container
lookupVolumeTopContainer vol = do
  row <- dbQuery1'
      $(makePGQuery
          (simpleQueryFlags { flagPrepare = Just [] })
          (   "SELECT container.id,container.top,container.name,container.date,slot_release.release"
           ++ " FROM container LEFT JOIN slot_release ON container.id = slot_release.container AND slot_release.segment = '(,)'"
           ++ " WHERE container.volume = ${volumeId $ volumeRow vol} ORDER BY container.id LIMIT 1" ))
  pure
     ((\(vid, tp, nm, dt, rl) -> 
        Container (ContainerRow vid tp nm dt) rl vol)
      row)

containerIsVolumeTop :: MonadDB c m => Container -> m Bool
containerIsVolumeTop Container{ containerRow = ContainerRow{ containerTop = False } } = return False
containerIsVolumeTop c = not <$>
  dbExecute1 [pgSQL|SELECT FROM container WHERE volume = ${volumeId $ volumeRow $ containerVolume c} AND id < ${containerId $ containerRow c} LIMIT 1|]

addContainer :: MonadAudit c m => Container -> m Container
addContainer bc = do
  ident <- getAuditIdentity
  dbQuery1' $(insertContainer 'ident 'bc)

changeContainer :: MonadAudit c m => Container -> m ()
changeContainer c = do
  ident <- getAuditIdentity
  dbExecute1' $(updateContainer 'ident 'c)

removeContainer :: MonadAudit c m => Container -> m Bool
removeContainer c = do
  ident <- getAuditIdentity
  top <- dbQuery1' [pgSQL|SELECT id FROM container WHERE volume = ${volumeId $ volumeRow $ containerVolume c} ORDER BY id LIMIT 1|]
  if top == containerId (containerRow c)
    then return False
    else isRight <$> dbTryJust (guard . isForeignKeyViolation) (dbExecute1 $(deleteContainer 'ident 'c))

getContainerDate :: Container -> Maybe MaskedDate
getContainerDate c = maskDateIf (dataPermission c == PermissionNONE) <$> containerDate (containerRow c)

formatContainerDate :: Container -> Maybe String
formatContainerDate c = formatTime defaultTimeLocale "%Y-%m-%d" <$> getContainerDate c

containerRowJSON :: JSON.ToObject o => ContainerRow -> JSON.Record (Id Container) o
containerRowJSON ContainerRow{..} = JSON.Record containerId $
     "top" JSON..=? (True <? containerTop)
  <> "name" JSON..=? containerName

containerJSON :: JSON.ToObject o => Container -> JSON.Record (Id Container) o
containerJSON c@Container{..} = containerRowJSON containerRow JSON..<>
     "date" JSON..=? formatContainerDate c
  <> "release" JSON..=? containerRelease

