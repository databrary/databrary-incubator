{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes, RecordWildCards, DataKinds #-}
module Databrary.Model.AssetSlot
  ( module Databrary.Model.AssetSlot.Types
  , lookupAssetSlot
  , lookupAssetAssetSlot
  , lookupSlotAssets
  , lookupContainerAssets
  , lookupVolumeAssetSlots
  , lookupVolumeAssetSlotIds
  , changeAssetSlot
  , changeAssetSlotDuration
  , fixAssetSlotDuration
  , findAssetContainerEnd
  , assetSlotName
  , assetSlotJSON
  ) where

import Control.Monad (when, guard)
import Data.Maybe (fromMaybe, isNothing)
import Data.Monoid ((<>))
import qualified Data.Text as T
import Database.PostgreSQL.Typed (pgSQL)
import Database.PostgreSQL.Typed.Query (makePGQuery, QueryFlags(..), simpleQueryFlags)

import Databrary.Ops
import Databrary.Has (peek, view)
import qualified Databrary.JSON as JSON
import Databrary.Service.DB
import Databrary.Model.Offset
import Databrary.Model.Permission
import Databrary.Model.Segment
import Databrary.Model.Id
import Databrary.Model.Party.Types
import Databrary.Model.Identity.Types
import Databrary.Model.Volume.Types
import Databrary.Model.Container.Types
import Databrary.Model.Slot.Types
import Databrary.Model.Asset
import Databrary.Model.Asset.SQL (makeAssetRow)
import Databrary.Model.Audit
import Databrary.Model.SQL
import Databrary.Model.AssetSlot.Types
import Databrary.Model.AssetSlot.SQL

$(useTDB)

lookupAssetSlot :: (MonadHasIdentity c m, MonadDB c m) => Id Asset -> m (Maybe AssetSlot)
lookupAssetSlot ai = do
  ident <- peek
  dbQuery1 $(selectQuery (selectAssetSlot 'ident) "$WHERE asset.id = ${ai}")

lookupAssetAssetSlot :: (MonadDB c m) => Asset -> m AssetSlot
lookupAssetAssetSlot a = fromMaybe assetNoSlot
  <$> dbQuery1 $(selectQuery selectAssetSlotAsset "$WHERE slot_asset.asset = ${assetId $ assetRow a} AND container.volume = ${volumeId $ volumeRow $ assetVolume a}")
  <*> return a

lookupSlotAssets :: (MonadDB c m) => Slot -> m [AssetSlot]
lookupSlotAssets (Slot c s) = do
  rows <- dbQuery   -- XXX volumes match?
      $(makePGQuery
          (simpleQueryFlags { flagPrepare = Just [] })
          (   "SELECT slot_asset.segment,asset.id,asset.format,asset.release,asset.duration,asset.name,asset.sha1,asset.size"
           ++ " FROM slot_asset JOIN asset ON slot_asset.asset = asset.id " 
           ++ "WHERE slot_asset.container = ${containerId $ containerRow c} AND slot_asset.segment && ${s} AND asset.volume = ${volumeId $ volumeRow $ containerVolume c}"))
  pure 
    (fmap 
       (\(sg,aid,fm,rl,dr,nm,sh,sz) -> 
          makeContainerSlotAsset
            sg 
            (makeAssetRow aid fm rl dr nm sh sz)
            c)
       rows)

lookupContainerAssets :: (MonadDB c m) => Container -> m [AssetSlot]
lookupContainerAssets = lookupSlotAssets . containerSlot

lookupVolumeAssetSlots :: (MonadDB c m) => Volume -> Bool -> m [AssetSlot]
lookupVolumeAssetSlots v top =
  dbQuery $ ($ v) <$> $(selectQuery selectVolumeSlotAsset "$WHERE asset.volume = ${volumeId $ volumeRow v} AND (container.top OR ${not top}) ORDER BY container.id")

lookupVolumeAssetSlotIds :: (MonadDB c m) => Volume -> m [(Asset, SlotId)]
lookupVolumeAssetSlotIds v = do
  rows <- dbQuery   -- XXX volumes match?
      $(makePGQuery
          (simpleQueryFlags { flagPrepare = Just [] })
          (   "SELECT slot_asset.container,slot_asset.segment,asset.id,asset.format,asset.release,asset.duration,asset.name,asset.sha1,asset.size"
           ++ " FROM slot_asset JOIN asset ON slot_asset.asset = asset.id " 
           ++ "WHERE asset.volume = ${volumeId $ volumeRow v} ORDER BY container"))
  pure 
    (fmap 
       (\(cn,sg,aid,fm,rl,dr,nm,sh,sz) -> 
          makeVolumeSlotIdAsset
            (SlotId cn sg)
            (makeAssetRow aid fm rl dr nm sh sz)
            v)
       rows)

changeAssetSlot :: (MonadAudit c m) => AssetSlot -> m Bool
changeAssetSlot as = do
  ident <- getAuditIdentity
  if isNothing (assetSlot as)
    then dbExecute1 $(deleteSlotAsset 'ident 'as)
    else do
      (r, _) <- updateOrInsert
        $(updateSlotAsset 'ident 'as)
        $(insertSlotAsset 'ident 'as)
      when (r /= 1) $ fail $ "changeAssetSlot: " ++ show r ++ " rows"
      return True

changeAssetSlotDuration :: MonadDB c m => Asset -> m Bool
changeAssetSlotDuration a
  | Just dur <- assetDuration $ assetRow a =
    dbExecute1 [pgSQL|UPDATE slot_asset SET segment = segment(lower(segment), lower(segment) + ${dur}) WHERE asset = ${assetId $ assetRow a}|]
  | otherwise = return False

fixAssetSlotDuration :: AssetSlot -> AssetSlot
fixAssetSlotDuration as
  | Just dur <- assetDuration $ assetRow $ slotAsset as = as{ assetSlot = (\s -> s{ slotSegment = segmentSetDuration dur (slotSegment s) }) <$> assetSlot as }
  | otherwise = as

findAssetContainerEnd :: MonadDB c m => Container -> m Offset
findAssetContainerEnd c = fromMaybe 0 <$>
  dbQuery1' [pgSQL|SELECT max(upper(segment))+'1s' FROM slot_asset WHERE container = ${containerId $ containerRow c}|]

assetSlotName :: AssetSlot -> Maybe T.Text
assetSlotName a = guard (any (containerTop . containerRow . slotContainer) (assetSlot a) || dataPermission a > PermissionNONE) >> assetName (assetRow $ slotAsset a)

assetSlotJSON :: JSON.ToObject o => AssetSlot -> JSON.Record (Id Asset) o
assetSlotJSON as@AssetSlot{..} = assetJSON slotAsset JSON..<>
  foldMap (segmentJSON . slotSegment) assetSlot
  --  "release" JSON..=? (view as :: Maybe Release)
  <> "name" JSON..=? assetSlotName as
  <> "permission" JSON..= p
  <> "size" JSON..=? (z <? p > PermissionNONE && any (0 <=) z)
  where
  p = dataPermission as
  z = assetSize $ assetRow slotAsset
