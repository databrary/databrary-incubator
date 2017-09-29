{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Databrary.Model.Asset.SQL
  ( selectAssetRow
  , selectAsset
  , insertAsset
  , updateAsset
  , makeAssetRow -- TODO: move to Types
  ) where

import qualified Data.ByteString as BS
import Data.Int (Int64)
import qualified Data.Text as T
import qualified Language.Haskell.TH as TH

import Databrary.Model.Offset
import Databrary.Model.Format
import Databrary.Model.Id.Types
import Databrary.Model.Release.Types
import Databrary.Model.SQL.Select
import Databrary.Model.Audit.SQL
import Databrary.Model.Volume.SQL
import Databrary.Model.Asset.Types

makeAssetRow :: Id Asset -> Id Format -> Maybe Release -> Maybe Offset -> Maybe T.Text -> Maybe BS.ByteString -> Maybe Int64 -> AssetRow
makeAssetRow i = AssetRow i . getFormat'

selectAssetRow :: Selector -- ^ @'AssetRow'@
selectAssetRow = selectColumns 'makeAssetRow "asset" ["id", "format", "release", "duration", "name", "sha1", "size"]

selectAsset :: TH.Name -- ^ @'Identity'@
  -> Selector -- ^ @'Asset'@
selectAsset ident = selectJoin 'Asset
  [ selectAssetRow
  , joinOn "asset.volume = volume.id" $ selectVolume ident
  ]

assetKeys :: String -- ^ @'Asset'@
  -> [(String, String)]
assetKeys r =
  [ ("id", "${assetId $ assetRow " ++ r ++ "}") ]

assetSets :: String -- ^ @'Asset'@
  -> [(String, String)]
assetSets a =
  [ ("volume", "${volumeId $ volumeRow $ assetVolume " ++ a ++ "}")
  , ("format", "${formatId $ assetFormat $ assetRow " ++ a ++ "}")
  , ("release", "${assetRelease $ assetRow " ++ a ++ "}")
  , ("duration", "${assetDuration $ assetRow " ++ a ++ "}")
  , ("name", "${assetName $ assetRow " ++ a ++ "}")
  , ("sha1", "${assetSHA1 $ assetRow " ++ a ++ "}")
  , ("size", "${assetSize $ assetRow " ++ a ++ "}")
  ]

setAssetId :: Asset -> Id Asset -> Asset
setAssetId a i = a{ assetRow = (assetRow a){ assetId = i } }

insertAsset :: TH.Name -- ^ @'AuditIdentity'@
  -> TH.Name -- ^ @'Asset'@
  -> TH.ExpQ -- ^ @'Asset'@
insertAsset ident a = auditInsert ident "asset"
  (assetSets (nameRef a))
  (Just $ selectOutput $ selectMap ((TH.VarE 'setAssetId `TH.AppE` TH.VarE a) `TH.AppE`) $ selectColumn "asset" "id")

updateAsset :: TH.Name -- ^ @'AuditIdentity'@
  -> TH.Name -- ^ @'Asset'@
  -> TH.ExpQ -- ^ @()@
updateAsset ident a = auditUpdate ident "asset"
  (assetSets (nameRef a))
  (whereEq $ assetKeys (nameRef a))
  Nothing
