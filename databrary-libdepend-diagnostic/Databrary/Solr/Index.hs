{-# LANGUAGE OverloadedStrings, RecordWildCards, GeneralizedNewtypeDeriving, ScopedTypeVariables, TemplateHaskell #-}
module Databrary.Solr.Index
  ( updateIndex
  ) where

import Control.Exception.Lifted (handle)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT(..), ask)
import Control.Monad.Trans.Class (lift)
import qualified Data.Aeson as JSON
import qualified Data.Aeson.Encode as JSON
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import Data.Foldable (fold)
import Data.Maybe (isNothing)
import Data.Monoid ((<>))
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import qualified Network.HTTP.Client as HC
import Network.HTTP.Types.Method (methodPost)
import Network.HTTP.Types.Header (hContentType)

import Control.Invert
import Databrary.Ops
import Databrary.Has
import Databrary.Service.Log
import Databrary.Model.Segment
import Databrary.Model.Kind
import Databrary.Model.Id.Types
import Databrary.Model.Permission.Types
import Databrary.Model.Party
import Databrary.Model.Volume.Types
import Databrary.Model.Citation
import Databrary.Model.Container
import Databrary.Model.Slot.Types
import Databrary.Model.Format.Types
import Databrary.Model.Asset.Types
import Databrary.Model.AssetSlot
import Databrary.Model.AssetSegment.Types
import Databrary.Model.Excerpt
import Databrary.Model.Category.Types
import Databrary.Model.Record.Types
import Databrary.Model.RecordSlot
import Databrary.Model.Measure
import Databrary.Model.Tag
import Databrary.Model.Comment
import Databrary.Context
import Databrary.Solr.Service
import Databrary.Solr.Document

solrDocId :: forall a . (Kinded a, Show (Id a)) => Id a -> BS.ByteString
solrDocId i = kindOf (undefined :: a) <> BSC.pack ('_' : show i)

solrParty :: Party -> Maybe Permission -> SolrDocument
solrParty Party{ partyRow = PartyRow{..}, ..} auth = SolrParty
  { solrId = solrDocId partyId
  , solrPartyId = partyId
  , solrPartySortName = partySortName
  , solrPartyPreName = partyPreName
  , solrPartyAffiliation = partyAffiliation
  , solrPartyIsInstitution = isNothing partyAccount
  , solrPartyAuthorization = auth
  }

solrVolume :: Volume -> Maybe Citation -> SolrDocument
solrVolume Volume{ volumeRow = VolumeRow{..}, ..} cite = SolrVolume
  { solrId = solrDocId volumeId
  , solrVolumeId = volumeId
  , solrName = Just volumeName
  , solrBody = volumeBody
  , solrVolumeOwnerIds = ownerIds
  , solrVolumeOwnerNames = ownerNames
  , solrCitation = citationHead <$> cite
  , solrCitationYear = citationYear =<< cite
  } where
  (ownerIds, ownerNames) = unzip volumeOwners

solrContainer :: Container -> SolrDocument
solrContainer c@Container{ containerRow = ContainerRow{..}, ..} = SolrContainer
  { solrId = solrDocId containerId
  , solrContainerId = containerId
  , solrVolumeId = volumeId $ volumeRow containerVolume
  , solrName = containerName
  , solrContainerTop = containerTop
  , solrContainerDate = getContainerDate c
  , solrRelease = containerRelease
  }

solrAsset :: AssetSlot -> SolrDocument
solrAsset as@AssetSlot{ slotAsset = Asset{ assetRow = AssetRow{..}, ..}, assetSlot = ~(Just Slot{..}) } = SolrAsset
  { solrId = solrDocId assetId
  , solrAssetId = assetId
  , solrVolumeId = volumeId $ volumeRow assetVolume
  , solrContainerId = containerId $ containerRow slotContainer
  , solrSegment = SolrSegment slotSegment
  , solrSegmentDuration = segmentLength slotSegment
  , solrName = assetSlotName as
  , solrRelease = assetRelease
  , solrFormatId = formatId assetFormat
  }

solrExcerpt :: Excerpt -> SolrDocument
solrExcerpt Excerpt{ excerptAsset = AssetSegment{ segmentAsset = AssetSlot{ slotAsset = Asset{ assetRow = AssetRow{..}, ..}, assetSlot = ~(Just Slot{ slotContainer = container }) }, assetSegment = seg }, ..} = SolrExcerpt
  { solrId = BSC.pack $ "excerpt_" <> show assetId
    <> maybe "" (('_':) . show) (lowerBound $ segmentRange seg)
  , solrAssetId = assetId
  , solrVolumeId = volumeId $ volumeRow assetVolume
  , solrContainerId = containerId $ containerRow container
  , solrSegment = SolrSegment seg
  , solrSegmentDuration = segmentLength seg
  , solrRelease = assetRelease
  }

solrRecord :: RecordSlot -> SolrDocument
solrRecord rs@RecordSlot{ slotRecord = r@Record{ recordRow = RecordRow{..}, ..}, recordSlot = Slot{..} } = SolrRecord
  { solrId = solrDocId recordId
    <> BSC.pack ('_' : show (containerId $ containerRow slotContainer))
  , solrRecordId = recordId
  , solrVolumeId = volumeId $ volumeRow recordVolume
  , solrContainerId = containerId $ containerRow slotContainer
  , solrSegment = SolrSegment slotSegment
  , solrSegmentDuration = segmentLength slotSegment
  , solrRecordCategoryId = categoryId recordCategory
  , solrRecordMeasures = SolrRecordMeasures $ map (\m -> (measureMetric m, measureDatum m)) $ getRecordMeasures r
  , solrRecordAge = recordSlotAge rs
  }

solrTag :: Tag -> SolrDocument
solrTag Tag{..} = SolrTagId
  { solrId = BSC.pack $ "tag_" <> show tagId
  , solrTagId = tagId
  , solrTagName = tagName
  }

solrTagUse :: Id Volume -> TagUseRow -> SolrDocument
solrTagUse vi TagUseRow{ useTagRow = Tag{..}, tagRowSlotId = SlotId{..}, ..} = SolrTag
  { solrId = BSC.pack $ "tag_" <> show tagId
    <> ('_' : show slotContainerId)
    <> (if tagRowKeyword then "" else '_' : show tagRowWhoId)
    <> maybe "" (('_':) . show) (lowerBound $ segmentRange slotSegmentId)
  , solrVolumeId = vi
  , solrContainerId = slotContainerId
  , solrSegment = SolrSegment slotSegmentId
  , solrSegmentDuration = segmentLength slotSegmentId
  , solrTagId = tagId
  , solrTagName = tagName
  , solrKeyword = tagRowKeyword ?> tagName
  , solrPartyId = tagRowWhoId
  }

solrComment :: Id Volume -> CommentRow -> SolrDocument
solrComment vi CommentRow{ commentRowSlotId = SlotId{..}, ..} = SolrComment
  { solrId = BSC.pack $ "comment_" <> show commentRowId
  , solrVolumeId = vi
  , solrContainerId = slotContainerId
  , solrSegment = SolrSegment slotSegmentId
  , solrSegmentDuration = segmentLength slotSegmentId
  , solrCommentId = commentRowId
  , solrPartyId = commentRowWhoId
  , solrBody = Just commentRowText
  }

type SolrM a = ReaderT BackgroundContext (InvertM BS.ByteString) a

writeBlock :: BS.ByteString -> SolrM ()
writeBlock = lift . give

writeDocuments :: [SolrDocument] -> SolrM ()
writeDocuments [] = return ()
writeDocuments d =
  writeBlock $ BSL.toStrict $ BSB.toLazyByteString $ foldMap (("},\"add\":{\"doc\":" <>) . JSON.encodeToBuilder . JSON.toJSON) d

writeUpdate :: SolrM () -> SolrM ()
writeUpdate f = do
  writeBlock "{\"delete\":{\"query\":\"*:*\""
  f
  writeBlock "},\"commit\":{\"waitSearcher\":true,\"expungeDeletes\":true},\"optimize\":{\"waitSearcher\":true}}"

joinContainers :: (a -> Slot -> b) -> [Container] -> [(a, SlotId)] -> [b]
joinContainers _ _ [] = []
joinContainers _ [] _ = error "joinContainers"
joinContainers f cl@(c:cr) al@((a, SlotId ci s):ar)
  | containerId (containerRow c) == ci = f a (Slot c s) : joinContainers f cl ar
  | otherwise = joinContainers f cr al

writeVolume :: (Volume, Maybe Citation) -> SolrM ()
writeVolume (v, vc) = do
  writeDocuments [solrVolume v vc]
  cl <- lookupVolumeContainers v
  writeDocuments $ map solrContainer cl
  writeDocuments . map solrAsset . joinContainers ((. Just) . AssetSlot) cl =<< lookupVolumeAssetSlotIds v
  -- this could be more efficient, but there usually aren't many:
  writeDocuments . map solrExcerpt =<< lookupVolumeExcerpts v
  writeDocuments . map solrRecord . joinContainers RecordSlot cl =<< lookupVolumeRecordSlotIds v
  writeDocuments . map (solrTagUse (volumeId $ volumeRow v)) =<< lookupVolumeTagUseRows v
  writeDocuments . map (solrComment (volumeId $ volumeRow v)) =<< lookupVolumeCommentRows v

writeAllDocuments :: SolrM ()
writeAllDocuments = do
  mapM_ writeVolume =<< lookupVolumesCitations
  writeDocuments . map (uncurry solrParty) =<< lookupPartyAuthorizations
  writeDocuments . map solrTag =<< lookupTags

updateIndex :: BackgroundContextM ()
updateIndex = do
  ctx <- ask
  req <- peeks solrRequest
  t <- liftIO getCurrentTime
  handle
    (\(e :: HC.HttpException) -> focusIO $ logMsg t ("solr update failed: " ++ show e))
    $ do
      _ <- focusIO $ HC.httpNoBody req
        { HC.path = HC.path req <> "update/json"
        , HC.method = methodPost
        , HC.requestBody = HC.RequestBodyStreamChunked $ \wf -> do
          w <- runInvert $ runReaderT (writeUpdate writeAllDocuments) ctx
          wf $ fold <$> w
        , HC.requestHeaders = (hContentType, "application/json") : HC.requestHeaders req
        , HC.responseTimeout = Just 100000000
        }
      t' <- liftIO getCurrentTime
      focusIO $ logMsg t' ("solr update complete " ++ show (diffUTCTime t' t))
