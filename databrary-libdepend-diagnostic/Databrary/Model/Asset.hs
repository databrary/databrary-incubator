{-# LANGUAGE OverloadedStrings, RecordWildCards, TemplateHaskell, QuasiQuotes, DataKinds #-}
module Databrary.Model.Asset
  ( module Databrary.Model.Asset.Types
  , blankAsset
  , assetBacked
  , lookupAsset
  , lookupVolumeAsset
  , addAsset
  , changeAsset
  , assetCreation
  , assetRowJSON
  , assetJSON
  ) where

import Control.Arrow (first)
import Data.Maybe (isNothing, isJust)
import Data.Monoid ((<>))
import qualified Data.Text as T
import Database.PostgreSQL.Typed (pgSQL)
import Database.PostgreSQL.Typed.Query (makePGQuery, QueryFlags(..), simpleQueryFlags)

import Databrary.Ops
import Databrary.Has (view, peek)
import qualified Databrary.JSON as JSON
import Databrary.Service.DB
import Databrary.Files
import Databrary.Store.Types
import Databrary.Store.Asset
import Databrary.Model.SQL
import Databrary.Model.Time
import Databrary.Model.Audit
import Databrary.Model.Id
import Databrary.Model.Identity
import Databrary.Model.Party
import Databrary.Model.Volume
import Databrary.Model.Volume.SQL (makeVolume, setCreation)
import Databrary.Model.Format
import Databrary.Model.Asset.Types
import Databrary.Model.Asset.SQL

$(useTDB)

blankAsset :: Volume -> Asset
blankAsset vol = Asset
  { assetRow = AssetRow
    { assetId = error "blankAsset"
    , assetFormat = unknownFormat
    , assetRelease = Nothing
    , assetName = Nothing
    , assetDuration = Nothing
    , assetSHA1 = Nothing
    , assetSize = Nothing
    }
  , assetVolume = vol
  }

assetBacked :: Asset -> Bool
assetBacked = isJust . assetSHA1 . assetRow

lookupAsset :: (MonadHasIdentity c m, MonadDB c m) => Id Asset -> m (Maybe Asset)
lookupAsset ai = do
  ident <- peek
  mRow <- dbQuery1
      $(makePGQuery
          (simpleQueryFlags { flagPrepare = Just [] })
          (   "SELECT asset.id,asset.format,asset.release,asset.duration,asset.name,asset.sha1,asset.size,volume.id,volume.name,volume.body,volume.alias,volume.doi,volume_creation(volume.id),volume_owners.owners,volume_permission.permission"
           ++ " FROM asset JOIN volume LEFT JOIN volume_owners ON volume.id = volume_owners.volume JOIN LATERAL (VALUES (CASE WHEN ${identitySuperuser ident} THEN enum_last(NULL::permission) ELSE volume_access_check(volume.id, ${view ident :: Id Party}) END)) AS volume_permission (permission) ON volume_permission.permission >= 'PUBLIC'::permission ON asset.volume = volume.id "
           ++ "WHERE asset.id = ${ai}" ))
  pure 
    (fmap 
       (\(aid,afm,arl,adr,anm,ash,asz,vid,vnm,vbd,vals,vdoi,vcr,vow,vpr) -> 
          ($)
            (Asset (makeAssetRow aid afm arl adr anm ash asz))
            (makeVolume
              (setCreation (VolumeRow vid vnm vbd vals vdoi) vcr)
              vow
              vpr))
       mRow)

lookupVolumeAsset :: (MonadDB c m) => Volume -> Id Asset -> m (Maybe Asset) -- TODO: expand soon
lookupVolumeAsset vol ai = do
  dbQuery1 $ (`Asset` vol) <$> $(selectQuery selectAssetRow "WHERE asset.id = ${ai} AND asset.volume = ${volumeId $ volumeRow vol}")

addAsset :: (MonadAudit c m, MonadStorage c m) => Asset -> Maybe RawFilePath -> m Asset
addAsset ba fp = do
  ident <- getAuditIdentity
  ba' <- maybe (return ba) (storeAssetFile ba) fp
  dbQuery1' $(insertAsset 'ident 'ba')

changeAsset :: (MonadAudit c m, MonadStorage c m) => Asset -> Maybe RawFilePath -> m Asset
changeAsset a fp = do
  ident <- getAuditIdentity
  a2 <- maybe (return a) (storeAssetFile a) fp
  dbExecute1' $(updateAsset 'ident 'a2)
  return a2

assetCreation :: MonadDB c m => Asset -> m (Maybe Timestamp, Maybe T.Text)
assetCreation a = maybe (Nothing, Nothing) (first Just) <$>
  dbQuery1 [pgSQL|$SELECT audit_time, name FROM audit.asset WHERE id = ${assetId $ assetRow a} AND audit_action = 'add' ORDER BY audit_time DESC LIMIT 1|]

assetRowJSON :: JSON.ToObject o => AssetRow -> JSON.Record (Id Asset) o
assetRowJSON AssetRow{..} = JSON.Record assetId $
     "format" JSON..= formatId assetFormat
  <> "classification" JSON..=? assetRelease
  <> "duration" JSON..=? assetDuration
  <> "pending" JSON..=? (isNothing assetSize <? isNothing assetSHA1)

assetJSON :: JSON.ToObject o => Asset -> JSON.Record (Id Asset) o
assetJSON Asset{..} = assetRowJSON assetRow
