{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Databrary.Model.AssetSlot.SQL
  ( slotAssetRow
  , makeSlotAsset
  , selectAssetSlotAsset
  , selectVolumeSlotAsset
  , selectSlotAsset
  , selectAssetSlot
  , insertSlotAsset
  , updateSlotAsset
  , deleteSlotAsset
  , makeContainerSlotAsset -- TODO: Move these to Types
  , makeVolumeSlotIdAsset 
  ) where

import Data.Maybe (fromMaybe)
import qualified Language.Haskell.TH as TH

import Databrary.Has (view)
import Databrary.Model.Segment
import Databrary.Model.Volume.Types
import Databrary.Model.Asset.Types
import Databrary.Model.Asset.SQL
import Databrary.Model.Container.Types
import Databrary.Model.Container.SQL
import Databrary.Model.Slot.Types
import Databrary.Model.SQL.Select
import Databrary.Model.Audit.SQL
import Databrary.Model.Volume.SQL
import Databrary.Model.AssetSlot.Types

slotAssetRow :: Selector -- ^ @'Segment'@
slotAssetRow = selectColumn "slot_asset" "segment"

makeSlotAsset :: Asset -> Container -> Segment -> AssetSlot
makeSlotAsset a c s = AssetSlot a (Just (Slot c s))

_selectAssetContainerSlotAsset :: Selector -- ^ @'Asset' -> 'Container' -> 'AssetSlot'@
_selectAssetContainerSlotAsset = selectMap (TH.VarE 'makeSlotAsset `TH.AppE`) slotAssetRow

makeContainerSlotAsset :: Segment -> AssetRow -> Container -> AssetSlot
makeContainerSlotAsset s ar c = makeSlotAsset (Asset ar $ view c) c s

makeVolumeSlotIdAsset :: SlotId -> AssetRow -> Volume -> (Asset, SlotId)
makeVolumeSlotIdAsset s ar v = (Asset ar v, s)

makeAssetSlotAsset :: Segment -> (Volume -> Container) -> Asset -> AssetSlot
makeAssetSlotAsset s cf a = makeSlotAsset a (cf (view a)) s

selectAssetSlotAsset :: Selector -- ^ @'Asset' -> 'AssetSlot'@
selectAssetSlotAsset = selectJoin 'makeAssetSlotAsset
  [ slotAssetRow
  , joinOn "slot_asset.container = container.id"
    selectVolumeContainer -- XXX volumes match?
  ]

makeVolumeSlotAsset :: Segment -> AssetRow -> (Volume -> Container) -> Volume -> AssetSlot
makeVolumeSlotAsset s ar cf v = makeSlotAsset (Asset ar v) (cf v) s

selectVolumeSlotAsset :: Selector -- ^ @'Volume' -> 'AssetSlot'@
selectVolumeSlotAsset = selectJoin 'makeVolumeSlotAsset
  [ slotAssetRow
  , joinOn "slot_asset.asset = asset.id"
    selectAssetRow
  , joinOn "slot_asset.container = container.id AND asset.volume = container.volume"
    selectVolumeContainer
  ]

selectSlotAsset :: TH.Name -- ^ @'Identity'@
  -> Selector -- ^ @'AssetSlot'@
selectSlotAsset ident = selectJoin '($)
  [ selectVolumeSlotAsset
  , joinOn "asset.volume = volume.id"
    $ selectVolume ident
  ]

makeVolumeAssetSlot :: AssetRow -> Maybe (Asset -> AssetSlot) -> Volume -> AssetSlot
makeVolumeAssetSlot ar sf = fromMaybe assetNoSlot sf . Asset ar

selectVolumeAssetSlot :: Selector -- ^ @'Volume' -> 'AssetSlot'@
selectVolumeAssetSlot = selectJoin 'makeVolumeAssetSlot
  [ selectAssetRow
  , maybeJoinOn "asset.id = slot_asset.asset AND asset.volume = container.volume"
    selectAssetSlotAsset
  ]

selectAssetSlot :: TH.Name -- ^ @'Identity'@
  -> Selector -- ^ @'AssetSlot'@
selectAssetSlot ident = selectJoin '($)
  [ selectVolumeAssetSlot
  , joinOn "asset.volume = volume.id"
    $ selectVolume ident
  ]

slotAssetKeys :: String -- ^ @'AssetSlot'@
  -> [(String, String)]
slotAssetKeys as =
  [ ("asset", "${assetId $ assetRow $ slotAsset " ++ as ++ "}") ]

slotAssetSets :: String -- ^ @'AssetSlot'@
  -> [(String, String)]
slotAssetSets as =
  [ ("container", "${containerId . containerRow . slotContainer <$> assetSlot " ++ as ++ "}")
  , ("segment", "${slotSegment <$> assetSlot " ++ as ++ "}")
  ]

insertSlotAsset :: TH.Name -- ^ @'AuditIdentity'@
  -> TH.Name -- ^ @'AssetSlot'@
  -> TH.ExpQ
insertSlotAsset ident o = auditInsert ident "slot_asset"
  (slotAssetKeys os ++ slotAssetSets os)
  Nothing
  where os = nameRef o

updateSlotAsset :: TH.Name -- ^ @'AuditIdentity'@
  -> TH.Name -- ^ @'AssetSlot'@
  -> TH.ExpQ
updateSlotAsset ident o = auditUpdate ident "slot_asset"
  (slotAssetSets os)
  (whereEq $ slotAssetKeys os)
  Nothing
  where os = nameRef o

deleteSlotAsset :: TH.Name -- ^ @'AuditIdentity'@
  -> TH.Name -- ^ @'AssetSlot'@
  -> TH.ExpQ
deleteSlotAsset ident o = auditDelete ident "slot_asset"
  (whereEq $ slotAssetKeys os)
  Nothing
  where os = nameRef o
