{-# LANGUAGE RecordWildCards, TemplateHaskell, QuasiQuotes, DataKinds #-}
module Databrary.EZID.Volume
  ( updateEZID
  ) where

import Control.Arrow ((&&&))
import Control.Monad ((<=<))
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.Function (on)
import Data.List (deleteFirstsBy)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Time.Clock (utctDay, getCurrentTime)
import Database.PostgreSQL.Typed.Query (pgSQL)
import qualified Network.Wai as Wai

import Databrary.Has
import Databrary.Service.DB
import Databrary.Service.Log
import Databrary.Context
import Databrary.Model.Time
import Databrary.Model.Permission
import Databrary.Model.Identity
import Databrary.Model.Id
import Databrary.Model.Volume
import Databrary.Model.VolumeAccess
import Databrary.Model.Citation
import Databrary.Model.Container
import Databrary.Model.Slot
import Databrary.Model.Funding
import Databrary.Model.Tag
import Databrary.Action.Route
import Databrary.Controller.Volume
import Databrary.EZID.API
import Databrary.EZID.DataCite

useTDB

volumeDOISuffix :: Id Volume -> BS.ByteString
volumeDOISuffix i = BSC.pack $ '.' : show i

volumeEZID :: (MonadDB c m, MonadHasIdentity c m) => Volume -> Maybe Citation -> m EZIDMeta
volumeEZID v@Volume{ volumeRow = VolumeRow{..}, ..} cite = do
  top <- lookupVolumeTopContainer v
  own <- lookupVolumeAccess v PermissionADMIN
  fund <- lookupVolumeFunding v
  link <- lookupVolumeLinks v
  key <- lookupSlotKeywords (containerSlot top)
  return EZIDPublic
    { ezidTarget = actionURI (Just Wai.defaultRequest) viewVolume (HTML, volumeId) []
    , ezidDataCite = DataCite
      { dataCiteDOI = volumeDOI
      , dataCiteTitle = volumeName
      , dataCiteAuthors = map volumeAccessParty own
      , dataCiteYear = dateYear (utctDay volumeCreation)
      , dataCiteDescription = volumeBody
      , dataCiteFunders = fund
      , dataCitePublication = citationURL =<< cite
      , dataCiteReferences = mapMaybe citationURL link
      , dataCiteSubjects = map (tagNameBS . tagName) key
      }
    }

lookupVolumeDOIs :: MonadDB c m => m [(Id Volume, BS.ByteString)]
lookupVolumeDOIs = dbQuery [pgSQL|!SELECT id, doi FROM volume WHERE doi IS NOT NULL|]

addVolumeDOI :: MonadDB c m => Id Volume -> BS.ByteString -> m Bool
addVolumeDOI v d = dbExecute1 [pgSQL|UPDATE volume SET doi = ${d} WHERE id = ${v} AND doi IS NULL|]

updateVolume :: Volume -> Maybe Citation -> EZIDM Bool
updateVolume v = maybe
  (maybe (return False) (addVolumeDOI $ volumeId $ volumeRow v) <=< ezidCreate (volumeDOISuffix $ volumeId $ volumeRow v))
  ezidModify
  (volumeDOI $ volumeRow v)
  <=< volumeEZID v

removeVolume :: Id Volume -> BS.ByteString -> EZIDM Bool
removeVolume _ d = ezidModify d EZIDUnavailable

updateEZID :: BackgroundContextM (Maybe Bool)
updateEZID = runEZIDM $ do
  r <- ezidStatus
  if r
    then do
      vl <- lookupVolumesCitations
      mapM_ (uncurry updateVolume) vl
      dl <- lookupVolumeDOIs
      mapM_ (uncurry removeVolume) $
        deleteFirstsBy (on (==) fst) dl (map ((volumeId &&& fromMaybe BS.empty . volumeDOI) . volumeRow . fst) vl)
    else do
      t <- liftIO getCurrentTime
      focusIO $ logMsg t "ezid is down"
  return r
