module Databrary.Model.AssetSegment.Types
  ( AssetSegment(..)
  , newAssetSegment
  , assetFullSegment
  , assetSlotSegment
  , assetSegmentFull
  , assetSegmentRange
  , Excerpt(..)
  , newExcerpt
  , excerptInSegment
  ) where

import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import qualified Database.PostgreSQL.Typed.Range as Range

import Databrary.Ops
import Databrary.Has (Has(..))
import Databrary.Model.Offset
import Databrary.Model.Segment
import Databrary.Model.Id.Types
import Databrary.Model.Permission.Types
import Databrary.Model.Volume.Types
import Databrary.Model.Release.Types
import Databrary.Model.Container.Types
import Databrary.Model.Slot.Types
import Databrary.Model.Format
import Databrary.Model.Asset.Types
import Databrary.Model.AssetSlot.Types

data AssetSegment = AssetSegment
  { segmentAsset :: AssetSlot
  , assetSegment :: !Segment
  , assetExcerpt :: Maybe Excerpt
  }

assetAssumedSegment :: AssetSlot -> Segment
assetAssumedSegment a
  | segmentFull seg = Segment $ Range.bounded 0 $ fromMaybe 0 $ assetDuration $ assetRow $ slotAsset a
  | otherwise = seg where seg = view a

-- |A "fake" (possibly invalid) 'AssetSegment' corresponding to the full 'AssetSlot'
assetSlotSegment :: AssetSlot -> AssetSegment
assetSlotSegment a = AssetSegment a (assetAssumedSegment a) Nothing

assetFullSegment :: AssetSegment -> AssetSegment
assetFullSegment AssetSegment{ assetExcerpt = Just e } = excerptFullSegment e
assetFullSegment AssetSegment{ segmentAsset = a } = assetSlotSegment a

newAssetSegment :: AssetSlot -> Segment -> Maybe Excerpt -> AssetSegment
newAssetSegment a s e = AssetSegment a (assetAssumedSegment a `segmentIntersect` s) e

assetSegmentFull :: AssetSegment -> Bool
assetSegmentFull AssetSegment{ segmentAsset = a, assetSegment = s } = assetAssumedSegment a == s

assetSegmentRange :: AssetSegment -> Range.Range Offset
assetSegmentRange AssetSegment{ segmentAsset = a, assetSegment = Segment s } =
  maybe id (fmap . subtract) (lowerBound $ segmentRange $ assetAssumedSegment a) s

instance Has AssetSlot AssetSegment where
  view = segmentAsset
instance Has Asset AssetSegment where
  view = view . segmentAsset
instance Has (Id Asset) AssetSegment where
  view = view . segmentAsset
instance Has Volume AssetSegment where
  view = view . segmentAsset
instance Has (Id Volume) AssetSegment where
  view = view . segmentAsset
instance Has Permission AssetSegment where
  view = view . segmentAsset

instance Has Slot AssetSegment where
  view AssetSegment{ segmentAsset = AssetSlot{ assetSlot = Just s }, assetSegment = seg } = s{ slotSegment = seg }
  view _ = error "unlinked AssetSegment"
instance Has Container AssetSegment where
  view = slotContainer . view
instance Has (Id Container) AssetSegment where
  view = containerId . containerRow . slotContainer . view
instance Has Segment AssetSegment where
  view = assetSegment

instance Has Format AssetSegment where
  view AssetSegment{ segmentAsset = a, assetSegment = Segment rng }
    | Just s <- formatSample fmt
    , Just _ <- assetDuration $ assetRow $ slotAsset a
    , Just _ <- Range.getPoint rng = s
    | otherwise = fmt
    where fmt = view a
instance Has (Id Format) AssetSegment where
  view = formatId . view

instance Has (Maybe Release) AssetSegment where
  view AssetSegment{ segmentAsset = a, assetExcerpt = Just e } = excerptRelease e <> view a
  view AssetSegment{ segmentAsset = a } = view a
instance Has Release AssetSegment where
  view = view . (view :: AssetSegment -> Maybe Release)


data Excerpt = Excerpt
  { excerptAsset :: !AssetSegment
  , excerptRelease :: !(Maybe Release)
  }

newExcerpt :: AssetSlot -> Segment -> Maybe Release -> Excerpt
newExcerpt a s r = e where
  as = newAssetSegment a s (Just e)
  e = Excerpt as r

excerptInSegment :: Excerpt -> Segment -> AssetSegment
excerptInSegment e@Excerpt{ excerptAsset = AssetSegment{ segmentAsset = a, assetSegment = es } } s
  | segmentOverlaps es s = as
  | otherwise = error "excerptInSegment: non-overlapping"
  where as = newAssetSegment a s (es `segmentContains` assetSegment as ?> e)

excerptFullSegment :: Excerpt -> AssetSegment
excerptFullSegment e = excerptInSegment e fullSegment

instance Has AssetSegment Excerpt where
  view = excerptAsset
instance Has AssetSlot Excerpt where
  view = view . excerptAsset
instance Has Asset Excerpt where
  view = view . excerptAsset
instance Has (Id Asset) Excerpt where
  view = view . excerptAsset
instance Has Volume Excerpt where
  view = view . excerptAsset
instance Has (Id Volume) Excerpt where
  view = view . excerptAsset
instance Has Slot Excerpt where
  view = view . excerptAsset
instance Has Container Excerpt where
  view = view . excerptAsset
instance Has (Id Container) Excerpt where
  view = view . excerptAsset
instance Has Segment Excerpt where
  view = view . excerptAsset
