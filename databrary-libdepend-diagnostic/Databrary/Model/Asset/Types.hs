{-# LANGUAGE OverloadedStrings, TemplateHaskell, TypeFamilies #-}
module Databrary.Model.Asset.Types
  ( AssetRow(..)
  , Asset(..)
  ) where

import qualified Data.ByteString as BS
import Data.Int (Int64)
import qualified Data.Text as T

import Databrary.Has (makeHasRec)
import Databrary.Model.Offset
import Databrary.Model.Kind
import Databrary.Model.Release.Types
import Databrary.Model.Id.Types
import Databrary.Model.Volume.Types
import Databrary.Model.Format.Types

type instance IdType Asset = Int32

data AssetRow = AssetRow
  { assetId :: Id Asset
  , assetFormat :: Format
  , assetRelease :: Maybe Release
  , assetDuration :: Maybe Offset
  , assetName :: Maybe T.Text
  , assetSHA1 :: Maybe BS.ByteString
  , assetSize :: Maybe Int64
  }

data Asset = Asset
  { assetRow :: !AssetRow
  , assetVolume :: Volume
  }

instance Kinded Asset where
  kindOf _ = "asset"

makeHasRec ''AssetRow ['assetId, 'assetFormat, 'assetRelease]
makeHasRec ''Asset ['assetRow, 'assetVolume]
