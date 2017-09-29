{-# LANGUAGE GeneralizedNewtypeDeriving, TemplateHaskell, OverloadedStrings #-}
module Databrary.Solr.Document
  ( SolrDocument(..)
  , SolrRecordMeasures(..)
  , SolrSegment(..)
  , metricField
  ) where

import qualified Data.Aeson as JSON
import qualified Data.Aeson.TH as JTH
import qualified Data.ByteString as BS
import Data.Char (isAlphaNum)
import Data.Int (Int16)
import qualified Data.HashMap.Strict as HM
import Data.Monoid ((<>))
import qualified Data.Text as T

import Databrary.Model.Id.Types
import Databrary.Model.Permission.Types
import Databrary.Model.Release.Types
import Databrary.Model.Party.Types
import Databrary.Model.Volume.Types
import Databrary.Model.Container.Types
import Databrary.Model.Offset
import Databrary.Model.Segment
import Databrary.Model.Format.Types
import Databrary.Model.Asset.Types
import Databrary.Model.Time
import Databrary.Model.Age
import Databrary.Model.Record.Types
import Databrary.Model.Category.Types
import Databrary.Model.Metric
import Databrary.Model.Tag.Types
import Databrary.Model.Comment.Types
import Databrary.String

safeField :: T.Text -> T.Text
safeField = T.map safeChar where
  safeChar c
    | isAlphaNum c = c
    | otherwise = '_'

newtype SolrRecordMeasures = SolrRecordMeasures [(Metric, MeasureDatum)]

metricLabel :: Metric -> T.Text
metricLabel Metric{ metricType = MeasureTypeText, metricOptions = _:_ } = "enum"
metricLabel m@Metric{ metricType = MeasureTypeText }
  | metricLong m = "long"
  | otherwise = "text"
metricLabel Metric{ metricType = MeasureTypeNumeric } = "numeric"
metricLabel Metric{ metricType = MeasureTypeDate } = "date"
metricLabel Metric{ metricType = MeasureTypeVoid } = "void"

metricField :: Metric -> T.Text
metricField m = "record_" <> metricLabel m <> ('_' `T.cons` safeField (metricName m))

-- slight hack because we actually index dates as datetimes
metricDatum :: Metric -> MeasureDatum -> JSON.Value
metricDatum Metric{ metricType = MeasureTypeDate } d = JSON.toJSON $ d <> "T12:00:00Z"
metricDatum Metric{ metricType = MeasureTypeVoid } _ = JSON.toJSON True
metricDatum _ d = JSON.toJSON d

measureKeyValue :: JSON.KeyValue kv => (Metric, MeasureDatum) -> kv
measureKeyValue (m, d) = metricField m JSON..= metricDatum m d

instance JSON.ToJSON SolrRecordMeasures where
  toJSON (SolrRecordMeasures ms) =
    JSON.object $ map measureKeyValue ms
  toEncoding (SolrRecordMeasures ms) =
    JSON.pairs $ foldr ((<>) . measureKeyValue) mempty ms

newtype SolrSegment = SolrSegment Segment deriving (JSON.FromJSON)

instance Show SolrSegment where
  showsPrec _ (SolrSegment s) = showSegmentWith (shows . offsetMillis) s

instance JSON.ToJSON SolrSegment where
  toJSON s = JSON.String $ T.pack $ show s
  toEncoding s = JSON.toEncoding $ show s

data SolrDocument
  = SolrParty
    { solrId :: !BS.ByteString
    , solrPartyId :: Id Party
    , solrPartySortName :: T.Text
    , solrPartyPreName :: Maybe T.Text
    , solrPartyAffiliation :: Maybe T.Text
    , solrPartyIsInstitution :: Bool
    , solrPartyAuthorization :: Maybe Permission
    }
  | SolrVolume
    { solrId :: !BS.ByteString
    , solrVolumeId :: Id Volume
    , solrName :: Maybe T.Text
    , solrBody :: Maybe T.Text -- body
    , solrVolumeOwnerIds :: [Id Party]
    , solrVolumeOwnerNames :: [T.Text]
    , solrCitation :: Maybe T.Text
    , solrCitationYear :: Maybe Int16
    }
  | SolrContainer
    { solrId :: !BS.ByteString
    , solrVolumeId :: Id Volume
    , solrContainerId :: Id Container
    , solrName :: Maybe T.Text
    , solrContainerTop :: Bool
    , solrContainerDate :: Maybe MaskedDate
    , solrRelease :: Maybe Release
    }
  | SolrAsset -- Slot
    { solrId :: !BS.ByteString
    , solrVolumeId :: Id Volume
    , solrContainerId :: Id Container
    , solrSegment :: SolrSegment
    , solrSegmentDuration :: Maybe Offset
    , solrAssetId :: Id Asset
    , solrName :: Maybe T.Text
    , solrFormatId :: Id Format
    , solrRelease :: Maybe Release
    }
  | SolrExcerpt
    { solrId :: !BS.ByteString
    , solrVolumeId :: Id Volume
    , solrContainerId :: Id Container
    , solrSegment :: SolrSegment
    , solrSegmentDuration :: Maybe Offset
    , solrAssetId :: Id Asset
    , solrRelease :: Maybe Release
    }
  | SolrRecord -- Slot
    { solrId :: !BS.ByteString
    , solrVolumeId :: Id Volume
    , solrContainerId :: Id Container
    , solrSegment :: SolrSegment
    , solrSegmentDuration :: Maybe Offset
    , solrRecordId :: Id Record
    , solrRecordCategoryId :: Id Category
    , solrRecordMeasures :: SolrRecordMeasures
    , solrRecordAge :: Maybe Age
    }
  | SolrTagId
    { solrId :: !BS.ByteString
    , solrTagId :: Id Tag
    , solrTagName :: TagName
    }
  | SolrTag -- Use
    { solrId :: !BS.ByteString
    , solrVolumeId :: Id Volume
    , solrContainerId :: Id Container
    , solrSegment :: SolrSegment
    , solrSegmentDuration :: Maybe Offset
    , solrTagId :: Id Tag
    , solrTagName :: TagName
    , solrKeyword :: Maybe TagName
    , solrPartyId :: Id Party
    }
  | SolrComment
    { solrId :: !BS.ByteString
    , solrVolumeId :: Id Volume
    , solrContainerId :: Id Container
    , solrSegment :: SolrSegment
    , solrSegmentDuration :: Maybe Offset
    , solrCommentId :: Id Comment
    , solrPartyId :: Id Party
    , solrBody :: Maybe T.Text
    }

$(return []) -- force new decl group for splice:

solrToJSON :: SolrDocument -> JSON.Value
solrToJSON =
  $(JTH.mkToJSON JTH.defaultOptions
    { JTH.fieldLabelModifier = \('s':'o':'l':'r':s) -> fromCamel s
    , JTH.constructorTagModifier = \('S':'o':'l':'r':s) -> fromCamel s
    , JTH.omitNothingFields = True
    , JTH.sumEncoding = JTH.TaggedObject
      { JTH.tagFieldName = "content_type"
      , JTH.contentsFieldName = error "solrToJSON: contentsFieldName"
      }
    } ''SolrDocument)

fixToJSON :: SolrDocument -> JSON.Value -> JSON.Value
fixToJSON SolrRecord{} (JSON.Object o) = JSON.Object $
  maybe o (HM.union $ HM.delete k o) $ do
    JSON.Object m <- HM.lookup k o
    return m
  where k = "record_measures"
fixToJSON _ j = j

instance JSON.ToJSON SolrDocument where
  toJSON s = fixToJSON s $ solrToJSON s
  -- TODO: toEncoding (and cleanup fixToJSON)
