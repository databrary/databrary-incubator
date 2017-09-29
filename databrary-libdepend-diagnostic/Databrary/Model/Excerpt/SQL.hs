{-# LANGUAGE TemplateHaskell #-}
module Databrary.Model.Excerpt.SQL
  ( makeExcerpt
  , makeAssetContainerExcerpt
  , makeContainerExcerpt -- TODO: this and above to Types
  , selectVolumeExcerpt
  , insertExcerpt
  , updateExcerpt
  , deleteExcerpt
  ) where

import qualified Language.Haskell.TH as TH

import Databrary.Model.SQL.Select
import Databrary.Model.Audit.SQL
import Databrary.Model.Release.Types
import Databrary.Model.Volume.Types
import Databrary.Model.Container.Types
import Databrary.Model.Container.SQL
import Databrary.Model.Segment
import Databrary.Model.Asset.Types
import Databrary.Model.Asset.SQL
import Databrary.Model.AssetSlot.Types
import Databrary.Model.AssetSlot.SQL
import Databrary.Model.AssetSegment.Types

makeExcerpt :: Segment -> Maybe Release -> AssetSlot -> Excerpt
makeExcerpt s r a = newExcerpt a s r

makeAssetContainerExcerpt :: Segment -> (AssetSlot -> Excerpt) -> Asset -> Container -> Excerpt
makeAssetContainerExcerpt as e a c = e $ makeSlotAsset a c as

selectAssetContainerExcerpt :: Selector -- ^ @'Asset' -> 'Container' -> 'Excerpt'@
selectAssetContainerExcerpt = selectJoin 'makeAssetContainerExcerpt
  [ selectColumn "slot_asset" "segment"
  , joinOn "slot_asset.asset = excerpt.asset"
    (selectColumns 'makeExcerpt "excerpt" ["segment", "release"])
  ]

makeContainerExcerpt :: (Asset -> Container -> Excerpt) -> AssetRow -> Container -> Excerpt
makeContainerExcerpt f ar c = f (Asset ar (containerVolume c)) c

makeVolumeExcerpt :: (Asset -> Container -> Excerpt) -> AssetRow -> (Volume -> Container) -> Volume -> Excerpt
makeVolumeExcerpt f ar cf v = f (Asset ar v) (cf v)

selectVolumeExcerpt :: Selector -- ^ @'Volume' -> 'Excerpt'@
selectVolumeExcerpt = selectJoin 'makeVolumeExcerpt
  [ selectAssetContainerExcerpt
  , joinOn "slot_asset.asset = asset.id"
     (selectColumns 'makeAssetRow "asset" ["id", "format", "release", "duration", "name", "sha1", "size"])
  , joinOn "slot_asset.container = container.id AND asset.volume = container.volume"
    selectVolumeContainer
  ]

excerptKeys :: String -- ^ @'Excerpt'@
  -> [(String, String)]
excerptKeys o =
  [ ("asset", "${assetId $ assetRow $ slotAsset $ segmentAsset $ excerptAsset " ++ o ++ "}")
  , ("segment", "${assetSegment $ excerptAsset " ++ o ++ "}")
  ]

excerptSets :: String -- ^ @'Excerpt'@
  -> [(String, String)]
excerptSets o =
  [ ("release", "${excerptRelease " ++ o ++ "}")
  ]

insertExcerpt :: TH.Name -- ^ @'AuditIdentity'@
  -> TH.Name -- ^ @'Excerpt'@
  -> TH.ExpQ
insertExcerpt ident o = auditInsert ident "excerpt"
  (excerptKeys os ++ excerptSets os)
  Nothing
  where os = nameRef o

updateExcerpt :: TH.Name -- ^ @'AuditIdentity'@
  -> TH.Name -- ^ @'Excerpt'@
  -> TH.ExpQ
updateExcerpt ident o = auditUpdate ident "excerpt"
  (excerptSets os)
  (whereEq $ excerptKeys os)
  Nothing
  where os = nameRef o

deleteExcerpt :: TH.Name -- ^ @'AuditIdentity'@
  -> TH.Name -- ^ @'AssetSegment'@
  -> TH.ExpQ
deleteExcerpt ident o = auditDelete ident "excerpt"
  ("asset = ${assetId $ assetRow $ slotAsset $ segmentAsset " ++ os ++ "} AND segment <@ ${assetSegment " ++ os ++ "}")
  Nothing
  where os = nameRef o
