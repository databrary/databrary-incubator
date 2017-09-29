{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes, RecordWildCards, ScopedTypeVariables, DataKinds #-}
module Databrary.Model.Volume
  ( module Databrary.Model.Volume.Types
  , coreVolume
  , lookupVolume
  , changeVolume
  , addVolume
  , auditVolumeDownload
  , VolumeFilter(..)
  , findVolumes
  , getVolumeAlias
  , volumeRowJSON
  , volumeJSON
  , updateVolumeIndex
  ) where

import Control.Applicative ((<|>))
import Control.Monad (guard)
import qualified Data.ByteString as BS
import Data.Monoid ((<>))
import qualified Data.Text as T
import Database.PostgreSQL.Typed.Query (pgSQL, unsafeModifyQuery)
import Database.PostgreSQL.Typed.Dynamic (pgLiteralRep)
import Database.PostgreSQL.Typed.Query (makePGQuery, QueryFlags(..), simpleQueryFlags)

import Databrary.Has (peek, view)
import Databrary.Service.DB
import qualified Databrary.JSON as JSON
import Databrary.Model.SQL (selectQuery)
import Databrary.Model.Paginate
import Databrary.Model.Id
import Databrary.Model.Permission
import Databrary.Model.Audit
import Databrary.Model.Party.Types
import Databrary.Model.Identity.Types
import Databrary.Model.Volume.Types
import Databrary.Model.Volume.SQL
import Databrary.Model.Volume.Boot

$(useTDB)

coreVolume :: Volume
coreVolume = $(loadVolume (Id 0))

lookupVolume :: (MonadDB c m, MonadHasIdentity c m) => Id Volume -> m (Maybe Volume)
lookupVolume vi = do
  ident :: Identity <- peek
  rows <- dbQuery1
      $(makePGQuery
          (simpleQueryFlags { flagPrepare = Just [] })
          (   "SELECT volume.id,volume.name,volume.body,volume.alias,volume.doi,volume_creation(volume.id),volume_owners.owners,volume_permission.permission"
           ++ " FROM volume LEFT JOIN volume_owners ON volume.id = volume_owners.volume JOIN LATERAL (VALUES (CASE WHEN ${identitySuperuser ident} THEN enum_last(NULL::permission) ELSE volume_access_check(volume.id, ${view ident :: Id Party}) END)) AS volume_permission (permission) ON volume_permission.permission >= 'PUBLIC'::permission "
           ++ " WHERE volume.id = ${vi} " ))
  pure 
    (fmap 
       (\(vid, nm, bd, als, doi, cr, own, prm) -> 
          makeVolume
            (setCreation (VolumeRow vid nm bd als doi) cr)
            own
            prm)
       rows)

changeVolume :: MonadAudit c m => Volume -> m ()
changeVolume v = do
  ident <- getAuditIdentity
  dbExecute1' $(updateVolume 'ident 'v)

addVolume :: MonadAudit c m => Volume -> m Volume
addVolume bv = do
  ident <- getAuditIdentity
  dbQuery1' $ fmap (\v -> v [] PermissionADMIN) $(insertVolume 'ident 'bv)

getVolumeAlias :: Volume -> Maybe T.Text
getVolumeAlias v = guard (volumePermission v >= PermissionREAD) >> volumeAlias (volumeRow v)

auditVolumeDownload :: MonadAudit c m => Bool -> Volume -> m ()
auditVolumeDownload success vol = do
  ai <- getAuditIdentity
  dbExecute1' [pgSQL|$INSERT INTO audit.volume (audit_action, audit_user, audit_ip, id) VALUES
    (${if success then AuditActionOpen else AuditActionAttempt}, ${auditWho ai}, ${auditIp ai}, ${volumeId $ volumeRow vol})|]

volumeRowJSON :: JSON.ToObject o => VolumeRow -> JSON.Record (Id Volume) o
volumeRowJSON VolumeRow{..} = JSON.Record volumeId $
     "name" JSON..= volumeName
  <> "body" JSON..= volumeBody

volumeJSON :: JSON.ToObject o => Volume -> JSON.Record (Id Volume) o
volumeJSON v@Volume{..} = volumeRowJSON volumeRow JSON..<>
     "doi" JSON..=? volumeDOI volumeRow
  <> "alias" JSON..=? getVolumeAlias v
  <> "creation" JSON..= volumeCreation
  <> "owners" JSON..= map (\(i, n) -> JSON.Object $ "id" JSON..= i <> "name" JSON..= n) volumeOwners
  <> "permission" JSON..= volumePermission

data VolumeFilter = VolumeFilter
  { volumeFilterQuery :: Maybe String
  , volumeFilterParty :: Maybe (Id Party)
  , volumeFilterPaginate :: Paginate
  }

instance Monoid VolumeFilter where
  mempty = VolumeFilter Nothing Nothing def
  mappend (VolumeFilter q1 p1 p) (VolumeFilter q2 p2 _) =
    VolumeFilter (q1 <> q2) (p1 <|> p2) p

volumeFilter :: VolumeFilter -> BS.ByteString
volumeFilter VolumeFilter{..} = BS.concat
  [ withq volumeFilterParty (const " JOIN volume_access ON volume.id = volume_access.volume")
  , withq volumeFilterQuery (\n -> " JOIN volume_text_idx ON volume.id = volume_text_idx.volume, plainto_tsquery('english', " <> pgLiteralRep n <> ") query")
  , " WHERE volume.id > 0 "
  , withq volumeFilterParty (\p -> " AND volume_access.party = " <> pgLiteralRep p <> " AND volume_access.individual >= 'EDIT'")
  , withq volumeFilterQuery (const " AND ts @@ query")
  , " ORDER BY "
  , withq volumeFilterQuery (const "ts_rank(ts, query) DESC,")
  , withq volumeFilterParty (const "volume_access.individual DESC,")
  , "volume.id DESC "
  , paginateSQL volumeFilterPaginate
  ]
  where
  withq v f = maybe BS.empty f v

findVolumes :: (MonadHasIdentity c m, MonadDB c m) => VolumeFilter -> m [Volume]
findVolumes pf = do
  ident <- peek
  rows <- dbQuery
    (unsafeModifyQuery
      $(makePGQuery
          (simpleQueryFlags { flagPrepare = Just [] })
          (   "SELECT volume.id,volume.name,volume.body,volume.alias,volume.doi,volume_creation(volume.id),volume_owners.owners,volume_permission.permission"
           ++ " FROM volume LEFT JOIN volume_owners ON volume.id = volume_owners.volume JOIN LATERAL (VALUES (CASE WHEN ${identitySuperuser ident} THEN enum_last(NULL::permission) ELSE volume_access_check(volume.id, ${view ident :: Id Party}) END)) AS volume_permission (permission) ON volume_permission.permission >= 'PUBLIC'::permission "
           ++ ""))
      (<> volumeFilter pf))
  pure 
    (fmap 
       (\(vid, nm, bd, als, doi, cr, own, prm) -> 
          makeVolume (setCreation (VolumeRow vid nm bd als doi) cr) own prm)
       rows)

updateVolumeIndex :: MonadDB c m => m ()
updateVolumeIndex =
  dbExecute_ "SELECT volume_text_refresh()"
