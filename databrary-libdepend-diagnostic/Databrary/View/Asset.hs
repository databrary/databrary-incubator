{-# LANGUAGE OverloadedStrings #-}
module Databrary.View.Asset
  ( htmlAssetEdit
  ) where

import Data.Foldable (fold)
import Data.Monoid ((<>))

import Databrary.Model.Volume
import Databrary.Model.Asset
import Databrary.Model.Container
import Databrary.Model.Slot
import Databrary.Model.AssetSlot
import Databrary.Action.Types
import Databrary.Action
import Databrary.View.Form

import {-# SOURCE #-} Databrary.Controller.Asset

htmlAssetForm :: Maybe Asset -> FormHtml f
htmlAssetForm asset = do
  field "name" $ inputText (assetName . assetRow =<< asset)
  field "classification" $ inputEnum True (assetRelease . assetRow =<< asset)
  field "file" inputFile
  -- TODO

htmlAssetEdit :: AssetTarget -> RequestContext -> FormHtml f
htmlAssetEdit (AssetTargetVolume v) = htmlForm "Create asset" createAsset     (HTML, volumeId $ volumeRow v) (htmlAssetForm Nothing) (const mempty)
htmlAssetEdit (AssetTargetSlot s)   = htmlForm "Create asset" createSlotAsset (HTML, slotId s) (field "container" (inputHidden $ show $ containerId $ containerRow $ slotContainer s) >> htmlAssetForm Nothing) (const mempty)
htmlAssetEdit (AssetTargetAsset t)  = htmlForm ("Edit asset " <> fold (assetName $ assetRow a)) postAsset (HTML, assetId $ assetRow a) (htmlAssetForm (Just a)) (const mempty)
  where a = slotAsset t
