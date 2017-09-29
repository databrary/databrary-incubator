{-# LANGUAGE OverloadedStrings, RecordWildCards, ScopedTypeVariables #-}
module Databrary.View.Zip
  ( htmlVolumeDescription
  ) where

import Control.Monad (void, unless, forM_)
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Char8 as BSC
import Data.Char (toLower)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.String (fromString)
import Data.Time.Format (formatTime, defaultTimeLocale)
import System.FilePath ((<.>))
import System.Posix.FilePath ((</>))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as HA
import qualified Text.Blaze.Html4.Strict.Attributes as H4A

import Databrary.Ops
import Databrary.Has (view)
import Databrary.Service.Messages
import Databrary.Store.Filename
import Databrary.Model.Time
import Databrary.Model.Enum
import Databrary.Model.Release.Types
import Databrary.Model.Id.Types
import Databrary.Model.Party
import Databrary.Model.Volume.Types
import Databrary.Model.Container
import Databrary.Model.Segment
import Databrary.Model.Slot.Types
import Databrary.Model.Citation.Types
import Databrary.Model.Funding.Types
import Databrary.Model.RecordSlot.Types
import Databrary.Model.Record.Types
import Databrary.Model.Category.Types
import Databrary.Model.Measure
import Databrary.Model.Metric.Types
import Databrary.Model.Asset.Types
import Databrary.Model.AssetSlot.Types
import Databrary.Model.Format
import Databrary.Action
import Databrary.Controller.Paths
import Databrary.Controller.Volume
import Databrary.Controller.Party
import Databrary.Controller.Container
import Databrary.Controller.Asset
import Databrary.Controller.Web
import Databrary.Controller.IdSet
import Databrary.View.Html

import {-# SOURCE #-} Databrary.Controller.Zip

htmlVolumeDescription :: Bool -> Volume -> [Citation] -> [Funding] -> [RecordSlot] -> IdSet Container -> [[AssetSlot]] -> [[AssetSlot]] -> RequestContext -> H.Html
htmlVolumeDescription inzip Volume{ volumeRow = VolumeRow{..}, ..} cite fund glob cs atl abl req = H.docTypeHtml $ do
  H.head $ do
    H.meta H.! HA.httpEquiv "content-type" H.! HA.content "text/html;charset=utf-8"
    H.title $ do
      void "Databrary Volume "
      H.toMarkup (unId volumeId)
  H.body $ do
    H.p $ do
      H.em "Databrary"
      void " Volume "
      H.toMarkup (unId volumeId)
      forM_ volumeDOI $ \doi -> do
        void " DOI "
        byteStringHtml doi
    H.h1 $
      H.a H.! HA.href (maybe (link viewVolume (HTML, volumeId)) (byteStringValue . ("http://doi.org/" <>)) volumeDOI) $
        H.text volumeName
    H.ul $ forM_ volumeOwners $ \(i, n) ->
      H.li $
        H.a H.! HA.href (link viewParty (HTML, TargetParty i)) $
          H.text n
    H.h2 "Volume description"
    mapM_ (H.p . H.text) volumeBody
    unless (null fund) $ do
      H.h3 "Funded by"
      H.dl $ forM_ fund $ \Funding{..} -> do
        H.dt $ H.text $ funderName fundingFunder
        mapM_ (H.dd . H.text) fundingAwards
    unless (null cite) $ do
      H.h3 "Related works"
      H.ul $ forM_ cite $ \Citation{..} -> H.li $
        maybe id (\u -> H.a H.! HA.href (H.toValue u)) citationURL $ do
          H.text citationHead
          forM_ citationYear $ \y ->
            " (" >> H.toMarkup (fromIntegral y :: Int) >> ")"
    unless (null glob) $ do
      H.h3 "Descriptors"
      forM_ glob $ \RecordSlot{ slotRecord = r, recordSlot = s } -> do
        H.h3 $ H.text $ categoryName $ recordCategory $ recordRow r
        H.dl $ do
          unless (segmentFull $ slotSegment s) $ do
            H.dt "segment"
            H.dd $ H.string $ show $ slotSegment s
          forM_ (getRecordMeasures r) $ \m -> do
            H.dt $ H.text $ metricName $ measureMetric m
            H.dd $ byteStringHtml $ measureDatum m
    H.h2 "Package information"
    H.dl $ do
      H.dt "Created"
      H.dd $ H.string $ formatTime defaultTimeLocale "%d %b %Y" volumeCreation
      if inzip
      then do
        H.dt "Downloaded"
        H.dd $ do
          H.string $ formatTime defaultTimeLocale "%a, %d %b %Y %H:%M:%S %Z" (view req :: Timestamp)
          void " by "
          H.a H.! HA.href (link viewParty (HTML, TargetParty $ view req)) $
            H.text $ partyName $ partyRow $ view req
      else do
        H.dt $ H.a H.! actionLink zipVolume volumeId (idSetQuery cs) $
          void "Download"
    unless (idSetIsFull cs) $ H.p $ msg "download.zip.partial"
    H.p $ do
      msg "download.warning"
      void " For more information and terms of use see the "
      H.a H.! HA.href "http://databrary.org/access/policies/agreement.html"
        $ "Databrary Access Agreement"
      void "."
    H.h2 "Contents"
    H.h3 "Legend of release levels"
    H.dl $ forM_ pgEnumValues $ \(_ :: Release, n) -> do
      H.dt $ H.string n
      H.dd $ do
        H.img H.! HA.src (link webFile (Just $ staticPath ["icons", "release", BSC.pack $ map toLower n <.> "svg"]))
        msg (fromString $ "release." ++ n ++ ".title")
        void ": "
        msg (fromString $ "release." ++ n ++ ".description")
    H.h3 "Materials"
    atable atl
    H.h3 "Sessions"
    atable abl
  where
  link r a = builderValue $ actionURL (inzip ?> view req) r a []
  msg m = H.text $ getMessage m $ view req
  atable acl = H.table H.! H4A.border "1" $ do
    H.thead $ H.tr $ do
      H.th "directory"
      H.th "container"
      H.th "file"
      H.th "description"
      H.th "release"
      H.th "size"
      H.th "duration"
      H.th "sha1 checksum"
    H.tbody $ abody acl
  abody [] = mempty
  abody (~(a@AssetSlot{ assetSlot = Just Slot{ slotContainer = c } }:l):al) = do
    H.tr $ do
      H.td H.! rs $ H.a !? (inzip ?> HA.href (byteStringValue fn)) $
        byteStringHtml dn
      H.td H.! rs $ H.a H.! HA.href (link viewContainer (HTML, (Just volumeId, containerId $ containerRow c))) $ do
        mapM_ H.string $ formatContainerDate c
        mapM_ H.text $ containerName $ containerRow c
      arow fn a
      mapM_ (H.tr . arow fn) l
    abody al
    where
    rs = HA.rowspan $ H.toValue $ succ $ length l
    dn = makeFilename $ containerDownloadName c
    fn
      | containerTop (containerRow c) = dn
      | otherwise = "sessions" </> dn
  arow bf as@AssetSlot{ slotAsset = Asset{ assetRow = a } } = do
    H.td $ H.a !? (inzip ?> HA.href (byteStringValue $ bf </> fn)) $
      byteStringHtml fn
    H.td $ H.a H.! HA.href (link viewAsset (HTML, assetId a)) $
      H.text $ fromMaybe (formatName $ assetFormat a) $ assetName a
    H.td $ H.string $ show (view as :: Release)
    H.td $ maybe mempty H.toMarkup $ assetSize a
    H.td $ maybe mempty (H.string . show) $ assetDuration a
    H.td $ maybe mempty (lazyByteStringHtml . BSB.toLazyByteString . BSB.byteStringHex) $ assetSHA1 a
    where
    fn = makeFilename (assetDownloadName a) `addFormatExtension` assetFormat a
