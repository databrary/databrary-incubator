{-# LANGUAGE OverloadedStrings, TypeFamilies #-}
module Databrary.Model.Transcode.Types
  ( Transcode(..)
  , TranscodePID
  , TranscodeArgs
  , transcodeAsset
  , transcodeOrig
  , transcodeId
  ) where

import qualified Data.ByteString as BS

import Databrary.Model.Kind
import Databrary.Model.Id.Types
import Databrary.Model.Time
import Databrary.Model.Segment
import Databrary.Model.Asset.Types
import Databrary.Model.AssetRevision.Types
import Databrary.Model.Party.Types

type TranscodePID = Int32
type TranscodeArgs = [String]

type instance IdType Transcode = Int32

data Transcode = Transcode
  { transcodeRevision :: !AssetRevision
  , transcodeOwner :: SiteAuth
  , transcodeSegment :: Segment
  , transcodeOptions :: TranscodeArgs
  , transcodeStart :: Maybe Timestamp
  , transcodeProcess :: Maybe TranscodePID
  , transcodeLog :: Maybe BS.ByteString
  }

transcodeAsset :: Transcode -> Asset
transcodeAsset = revisionAsset . transcodeRevision

transcodeOrig :: Transcode -> Asset
transcodeOrig = revisionOrig . transcodeRevision

transcodeId :: Transcode -> Id Transcode
transcodeId = Id . unId . assetId . assetRow . transcodeAsset

instance Kinded Transcode where
  kindOf _ = "transcode"

