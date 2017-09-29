{-# LANGUAGE OverloadedStrings, ViewPatterns #-}
module Databrary.Controller.Zip
  ( zipContainer
  , zipVolume
  , viewVolumeDescription
  ) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import Data.Function (on)
import Data.List (groupBy, partition)
import Data.Maybe (fromJust, maybeToList)
import Data.Monoid ((<>))
import qualified Data.RangeSet.List as RS
import qualified Data.Text.Encoding as TE
import Network.HTTP.Types (hContentType, hCacheControl, hContentLength)
import System.Posix.FilePath ((<.>))
import qualified Text.Blaze.Html5 as Html
import qualified Text.Blaze.Html.Renderer.Utf8 as Html

import Databrary.Ops
import Databrary.Has (view, peek, peeks)
import Databrary.Store.Asset
import Databrary.Store.Filename
import Databrary.Store.CSV (buildCSV)
import Databrary.Store.Zip
import Databrary.Model.Id
import Databrary.Model.Permission
import Databrary.Model.Volume
import Databrary.Model.Container
import Databrary.Model.Slot
import Databrary.Model.RecordSlot
import Databrary.Model.Asset
import Databrary.Model.AssetSlot
import Databrary.Model.Format
import Databrary.Model.Party
import Databrary.Model.Citation
import Databrary.Model.Funding
import Databrary.HTTP
import Databrary.HTTP.Path.Parser
import Databrary.Action
import Databrary.Controller.Paths
import Databrary.Controller.Asset
import Databrary.Controller.Container
import Databrary.Controller.Volume
import Databrary.Controller.Party
import Databrary.Controller.CSV
import Databrary.Controller.Angular
import Databrary.Controller.IdSet
import Databrary.View.Zip

assetZipEntry :: AssetSlot -> ActionM ZipEntry
assetZipEntry AssetSlot{ slotAsset = a@Asset{ assetRow = ar } } = do
  Just f <- getAssetFile a
  req <- peek
  -- (t, _) <- assetCreation a
  -- Just (t, s) <- fileInfo f
  return blankZipEntry
    { zipEntryName = makeFilename (assetDownloadName ar) `addFormatExtension` assetFormat ar
    , zipEntryTime = Nothing
    , zipEntryComment = BSL.toStrict $ BSB.toLazyByteString $ actionURL (Just req) viewAsset (HTML, assetId ar) []
    , zipEntryContent = ZipEntryFile (fromIntegral $ fromJust $ assetSize ar) f
    }

containerZipEntry :: Container -> [AssetSlot] -> ActionM ZipEntry
containerZipEntry c l = do
  req <- peek
  a <- mapM assetZipEntry l
  return blankZipEntry
    { zipEntryName = makeFilename (containerDownloadName c)
    , zipEntryComment = BSL.toStrict $ BSB.toLazyByteString $ actionURL (Just req) viewContainer (HTML, (Nothing, containerId $ containerRow c)) []
    , zipEntryContent = ZipDirectory a
    }

volumeDescription :: Bool -> Volume -> (Container, [RecordSlot]) -> IdSet Container -> [AssetSlot] -> ActionM (Html.Html, [[AssetSlot]], [[AssetSlot]])
volumeDescription inzip v (_, glob) cs al = do
  cite <- lookupVolumeCitation v
  links <- lookupVolumeLinks v
  fund <- lookupVolumeFunding v
  desc <- peeks $ htmlVolumeDescription inzip v (maybeToList cite ++ links) fund glob cs at ab
  return (desc, at, ab)
  where
  (at, ab) = partition (any (containerTop . containerRow . slotContainer) . assetSlot . head) $ groupBy (me `on` fmap (containerId . containerRow . slotContainer) . assetSlot) al
  me (Just x) (Just y) = x == y
  me _ _ = False

volumeZipEntry :: Volume -> (Container, [RecordSlot]) -> IdSet Container -> Maybe BSB.Builder -> [AssetSlot] -> ActionM ZipEntry
volumeZipEntry v top cs csv al = do
  req <- peek
  (desc, at, ab) <- volumeDescription True v top cs al
  zt <- mapM ent at
  zb <- mapM ent ab
  return blankZipEntry
    { zipEntryName = makeFilename $ volumeDownloadName v ++ if idSetIsFull cs then [] else ["PARTIAL"]
    , zipEntryComment = BSL.toStrict $ BSB.toLazyByteString $ actionURL (Just req) viewVolume (HTML, volumeId $ volumeRow v) []
    , zipEntryContent = ZipDirectory
      $ blankZipEntry
        { zipEntryName = "description.html"
        , zipEntryContent = ZipEntryPure $ Html.renderHtml $ desc
        }
      : maybe id (\c -> (blankZipEntry
        { zipEntryName = "spreadsheet.csv"
        , zipEntryContent = ZipEntryPure $ BSB.toLazyByteString c
        } :)) csv
      (if null zb then zt else (zt ++ [blankZipEntry
        { zipEntryName = "sessions"
        , zipEntryContent = ZipDirectory zb
        }]))
    }
  where
  ent [a@AssetSlot{ assetSlot = Nothing }] = assetZipEntry a
  ent l@(AssetSlot{ assetSlot = Just s } : _) = containerZipEntry (slotContainer s) l
  ent _ = fail "volumeZipEntry"

zipResponse :: BS.ByteString -> [ZipEntry] -> ActionM Response
zipResponse n z = do
  req <- peek
  u <- peek
  let comment = BSL.toStrict $ BSB.toLazyByteString
        $ BSB.string8 "Downloaded by " <> TE.encodeUtf8Builder (partyName $ partyRow u) <> BSB.string8 " <" <> actionURL (Just req) viewParty (HTML, TargetParty $ partyId $ partyRow u) [] <> BSB.char8 '>'
  return $ okResponse
    [ (hContentType, "application/zip")
    , ("content-disposition", "attachment; filename=" <> quoteHTTP (n <.> "zip"))
    , (hCacheControl, "max-age=31556926, private")
    , (hContentLength, BSC.pack $ show $ sizeZip z + fromIntegral (BS.length comment))
    ] (streamZip z comment)

zipEmpty :: ZipEntry -> Bool
zipEmpty ZipEntry{ zipEntryContent = ZipDirectory l } = all zipEmpty l
zipEmpty _ = False

checkAsset :: AssetSlot -> Bool
checkAsset a = dataPermission a > PermissionNONE && assetBacked (view a)

zipContainer :: ActionRoute (Maybe (Id Volume), Id Slot)
zipContainer = action GET (pathMaybe pathId </> pathSlotId </< "zip") $ \(vi, ci) -> withAuth $ do
  c <- getContainer PermissionPUBLIC vi ci True
  z <- containerZipEntry c . filter checkAsset =<< lookupContainerAssets c
  auditSlotDownload (not $ zipEmpty z) (containerSlot c)
  zipResponse ("databrary-" <> BSC.pack (show $ volumeId $ volumeRow $ containerVolume c) <> "-" <> BSC.pack (show $ containerId $ containerRow c)) [z]

getVolumeInfo :: Id Volume -> ActionM (Volume, IdSet Container, [AssetSlot])
getVolumeInfo vi = do
  v <- getVolume PermissionPUBLIC vi
  s <- peeks requestIdSet
  a <- filter (\a@AssetSlot{ assetSlot = Just c } -> checkAsset a && RS.member (containerId $ containerRow $ slotContainer c) s) <$>
    lookupVolumeAssetSlots v False
  return (v, s, a)

zipVolume :: ActionRoute (Id Volume)
zipVolume = action GET (pathId </< "zip") $ \vi -> withAuth $ do
  (v, s, a) <- getVolumeInfo vi
  top:cr <- lookupVolumeContainersRecords v
  let cr' = filter ((`RS.member` s) . containerId . containerRow . fst) cr
  csv <- null cr' ?!$> volumeCSV v cr'
  z <- volumeZipEntry v top s (buildCSV <$> csv) a
  auditVolumeDownload (not $ null a) v
  zipResponse (BSC.pack $ "databrary-" ++ show (volumeId $ volumeRow v) ++ if idSetIsFull s then "" else "-partial") [z]

viewVolumeDescription :: ActionRoute (Id Volume)
viewVolumeDescription = action GET (pathId </< "description") $ \vi -> withAuth $ do
  angular
  (v, s, a) <- getVolumeInfo vi
  top <- lookupVolumeTopContainer v
  glob <- lookupSlotRecords $ containerSlot top
  (desc, _, _) <- volumeDescription False v (top, glob) s a
  return $ okResponse [] desc
