{-# LANGUAGE TemplateHaskell #-}
module Databrary.Model.Transcode.SQL
  ( selectOrigTranscode
  , selectTranscode
  , makeTranscodeRow -- TODO: move to types
  , makeOrigTranscode
  ) where

import qualified Data.ByteString as BS
import Data.Int (Int32)
import Data.Maybe (fromMaybe)
import qualified Language.Haskell.TH as TH

import Databrary.Model.SQL.Select
import Databrary.Model.Time
import Databrary.Model.Permission.Types
import Databrary.Model.Party.Types
import Databrary.Model.Party.SQL
import Databrary.Model.Volume.Types
import Databrary.Model.Volume.SQL
import Databrary.Model.Asset.Types
import Databrary.Model.Asset.SQL
import Databrary.Model.Segment
import Databrary.Model.AssetRevision.Types
import Databrary.Model.Transcode.Types

makeTranscodeRow :: Segment -> [Maybe String] -> Maybe Timestamp -> Maybe Int32 -> Maybe BS.ByteString -> SiteAuth -> AssetRevision -> Transcode
makeTranscodeRow s f t p l u a =
  Transcode a u s (map (fromMaybe (error "NULL transcode options")) f) t p l

selectAssetRevisionTranscode :: Selector -- ^ @'AssetRevision' -> 'Transcode'@
selectAssetRevisionTranscode = selectJoin '($)
  [ selectColumns 'makeTranscodeRow "transcode" ["segment", "options", "start", "process", "log"]
  , joinOn "transcode.owner = party.id"
    selectSiteAuth
  ]

makeOrigTranscode :: (AssetRevision -> Transcode) -> AssetRow -> Asset -> Transcode
makeOrigTranscode f a o = f $ AssetRevision (Asset a $ assetVolume o) o

selectOrigTranscode :: Selector -- ^ @'Asset' -> 'Transcode'@
selectOrigTranscode = selectJoin 'makeOrigTranscode
  [ selectAssetRevisionTranscode
  , joinOn "transcode.asset = asset.id"
    (selectColumns 'makeAssetRow "asset" ["id", "format", "release", "duration", "name", "sha1", "size"])
  ]

makeTranscode :: (Asset -> Transcode) -> AssetRow -> (Permission -> Volume) -> Transcode
makeTranscode t o vp = t $ Asset o $ vp PermissionADMIN

selectTranscode :: Selector -- ^ @'Transcode'@
selectTranscode = selectJoin 'makeTranscode
  [ selectOrigTranscode
  , joinOn "transcode.orig = orig.id"
    $ selectAssetRow `fromAlias` "orig"
  , selectMap (`TH.AppE` TH.ListE [])
    $ joinOn "asset.volume = volume.id AND orig.volume = volume.id"
      selectPermissionVolume
  ]
