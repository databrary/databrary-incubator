{-# LANGUAGE OverloadedStrings #-}
module Databrary.Controller.Asset
  ( getAsset
  , assetJSONField
  , viewAsset
  , AssetTarget(..)
  , postAsset
  , viewAssetEdit
  , createAsset
  , viewAssetCreate
  , createSlotAsset
  , viewSlotAssetCreate
  , deleteAsset
  , downloadAsset
  , thumbAsset
  , assetDownloadName
  ) where

import Control.Applicative ((<|>))
import Control.Monad ((<=<), void, guard, when)
import Control.Monad.Trans.Class (lift)
import qualified Data.ByteString as BS
import Data.Maybe (fromMaybe, isNothing, isJust, maybeToList)
import Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Database.PostgreSQL.Typed.Range as Range
import Network.HTTP.Types (conflict409)
import qualified Network.Wai as Wai
import Network.Wai.Parse (FileInfo(..))

import Databrary.Ops
import Databrary.Has
import qualified Databrary.JSON as JSON
import Databrary.Model.Segment
import Databrary.Model.Permission
import Databrary.Model.Release
import Databrary.Model.Id
import Databrary.Model.Volume
import Databrary.Model.Container
import Databrary.Model.Token
import Databrary.Model.Format
import Databrary.Model.Asset
import Databrary.Model.Slot
import Databrary.Model.AssetSlot
import Databrary.Model.AssetSegment
import Databrary.Model.Excerpt
import Databrary.Model.AssetRevision
import Databrary.Model.Transcode
import Databrary.Model.Notification
import Databrary.Files hiding ((</>))
import Databrary.Store.Types
import Databrary.Store.Asset
import Databrary.Store.Upload
import Databrary.Store.Temp
import Databrary.Store.Transcode
import Databrary.Store.AV (avProbeLength)
import Databrary.Store.Probe
import Databrary.HTTP.Request
import Databrary.HTTP.Form.Errors
import Databrary.HTTP.Form.Deform
import Databrary.HTTP.Path.Parser
import Databrary.Action
import Databrary.Controller.Paths
import Databrary.Controller.Permission
import Databrary.Controller.Form
import Databrary.Controller.Volume
import Databrary.Controller.Slot
import Databrary.Controller.Format
import Databrary.Controller.Notification
import {-# SOURCE #-} Databrary.Controller.AssetSegment
import Databrary.View.Asset

getAsset :: Permission -> Id Asset -> ActionM AssetSlot
getAsset p i =
  checkPermission p =<< maybeAction =<< lookupAssetSlot i

assetJSONField :: AssetSlot -> BS.ByteString -> Maybe BS.ByteString -> ActionM (Maybe JSON.Encoding)
assetJSONField a "container" _ =
  return $ JSON.recordEncoding . containerJSON . slotContainer <$> assetSlot a
assetJSONField a "creation" _ | view a >= PermissionEDIT = do
  (t, n) <- assetCreation $ slotAsset a
  return $ Just $ JSON.objectEncoding $
       "date" JSON..=? t
    <> "name" JSON..=? n
assetJSONField a "excerpts" _ =
  Just . JSON.mapObjects excerptJSON <$> lookupAssetExcerpts a
assetJSONField _ _ _ = return Nothing

assetJSONQuery :: AssetSlot -> JSON.Query -> ActionM (JSON.Record (Id Asset) JSON.Series)
assetJSONQuery o q = (assetSlotJSON o JSON..<>) <$> JSON.jsonQuery (assetJSONField o) q

assetDownloadName :: AssetRow -> [T.Text]
assetDownloadName a = T.pack (show $ assetId a) : maybeToList (assetName a)

viewAsset :: ActionRoute (API, Id Asset)
viewAsset = action GET (pathAPI </> pathId) $ \(api, i) -> withAuth $ do
  asset <- getAsset PermissionPUBLIC i
  case api of
    JSON -> okResponse [] <$> (assetJSONQuery asset =<< peeks Wai.queryString)
    HTML
      | Just s <- assetSlot asset -> peeks $ otherRouteResponse [] viewAssetSegment (api, Just (view asset), slotId s, assetId $ assetRow $ slotAsset asset)
      | otherwise -> return $ okResponse [] $ T.pack $ show $ assetId $ assetRow $ slotAsset asset -- TODO

data AssetTarget
  = AssetTargetVolume Volume
  | AssetTargetSlot Slot
  | AssetTargetAsset AssetSlot

data FileUploadFile
  = FileUploadForm (FileInfo TempFile)
  | FileUploadToken Upload

fileUploadName :: FileUploadFile -> BS.ByteString
fileUploadName (FileUploadForm f) = fileName f
fileUploadName (FileUploadToken u) = uploadFilename u

fileUploadPath :: FileUploadFile -> Storage -> RawFilePath
fileUploadPath (FileUploadForm f) _ = tempFilePath $ fileContent f
fileUploadPath (FileUploadToken u) s = uploadFile u s

fileUploadRemove :: FileUploadFile -> ActionM ()
fileUploadRemove (FileUploadForm f) = focusIO $ releaseTempFile $ fileContent f
fileUploadRemove (FileUploadToken u) = void $ removeUpload u

data FileUpload = FileUpload
  { fileUploadFile :: FileUploadFile
  , fileUploadProbe :: Probe
  }

deformLookup :: (Monad m, Functor m, Deform f a) => FormErrorMessage -> (a -> m (Maybe b)) -> DeformT f m (Maybe b)
deformLookup e l = mapM (deformMaybe' e <=< lift . l) =<< deformNonEmpty deform

detectUpload :: FileUploadFile -> DeformActionM TempFile FileUpload
detectUpload u =
  either deformError' (return . FileUpload u)
    =<< lift (probeFile (fileUploadName u) =<< peeks (fileUploadPath u))

processAsset :: API -> AssetTarget -> ActionM Response
processAsset api target = do
  let as@AssetSlot{ slotAsset = a, assetSlot = s } = case target of
        AssetTargetVolume t -> assetNoSlot $ blankAsset t
        AssetTargetSlot t -> AssetSlot (blankAsset (view t)) (Just t)
        AssetTargetAsset t -> t
  (as', up') <- runFormFiles [("file", maxAssetSize)] (api == HTML ?> htmlAssetEdit target) $ do
    csrfForm
    file <- "file" .:> deform
    upload <- "upload" .:> deformLookup "Uploaded file not found." lookupUpload
    upfile <- case (file, upload) of
      (Just f, Nothing) -> return $ Just $ FileUploadForm f
      (Nothing, Just u) -> return $ Just $ FileUploadToken u
      (Nothing, Nothing)
        | AssetTargetAsset _ <- target -> return Nothing
        | otherwise -> Nothing <$ deformError "File or upload required."
      _ -> Nothing <$ deformError "Conflicting uploaded files found."
    up <- mapM detectUpload upfile
    let fmt = maybe (assetFormat $ assetRow a) (probeFormat . fileUploadProbe) up
    name <- "name" .:> maybe (assetName $ assetRow a) (TE.decodeUtf8 . dropFormatExtension fmt <$>) <$> deformOptional (deformNonEmpty deform)
    classification <- "classification" .:> fromMaybe (assetRelease $ assetRow a) <$> deformOptional (deformNonEmpty deform)
    slot <-
      "container" .:> (<|> slotContainer <$> s) <$> deformLookup "Container not found." (lookupVolumeContainer (assetVolume a))
      >>= mapM (\c -> "position" .:> do
        let seg = slotSegment <$> s
            dur = maybe (assetDuration $ assetRow a) (probeLength . fileUploadProbe) up
        p <- fromMaybe (lowerBound . segmentRange =<< seg) <$> deformOptional (deformNonEmpty deform)
        Slot c . maybe fullSegment
          (\l -> Segment $ Range.bounded l (l + fromMaybe 0 ((segmentLength =<< seg) <|> dur)))
          <$> orElseM p (mapM (lift . probeAutoPosition c . Just . fileUploadProbe) (guard (isNothing s && isJust dur) >> up)))
    return
      ( as
        { slotAsset = a
          { assetRow = (assetRow a)
            { assetName = name
            , assetRelease = classification
            , assetFormat = fmt
            }
          }
        , assetSlot = slot
        }
      , up
      )
  as'' <- maybe (return as') (\up@FileUpload{ fileUploadFile = upfile } -> do
    a' <- addAsset (slotAsset as')
      { assetRow = (assetRow $ slotAsset as')
        { assetName = Just $ TE.decodeUtf8 $ fileUploadName upfile
        , assetDuration = Nothing
        , assetSize = Nothing
        , assetSHA1 = Nothing
        }
      } . Just =<< peeks (fileUploadPath upfile)
    fileUploadRemove upfile
    td <- checkAlreadyTranscoded a' (fileUploadProbe up)
    te <- peeks transcodeEnabled
    t <- case fileUploadProbe up of
      ProbeAV{ probeAV = av } | td ->
        return a'{ assetRow = (assetRow a'){ assetDuration = avProbeLength av } }
      probe@ProbeAV{} | te -> do
        t <- addTranscode a' fullSegment defaultTranscodeOptions probe
        _ <- forkTranscode t
        return $ transcodeAsset t
      _ -> return a'
    case target of
      AssetTargetAsset _ -> replaceAsset a t
      _ -> return ()
    return $ fixAssetSlotDuration as'
      { slotAsset = t
        { assetRow = (assetRow t)
          { assetName = assetName $ assetRow $ slotAsset as'
          }
        }
      })
    up'
  a' <- changeAsset (slotAsset as'') Nothing
  _ <- changeAssetSlot as''
  when (assetRelease (assetRow a') == Just ReleasePUBLIC && assetRelease (assetRow a) /= Just ReleasePUBLIC) $
    createVolumeNotification (assetVolume a') $ \n -> (n NoticeReleaseAsset)
      { notificationContainerId = containerId . containerRow . slotContainer <$> assetSlot as''
      , notificationSegment = slotSegment <$> assetSlot as''
      , notificationAssetId = Just $ assetId $ assetRow a'
      , notificationRelease = assetRelease $ assetRow a'
      }
  case api of
    JSON -> return $ okResponse [] $ JSON.recordEncoding $ assetSlotJSON as''
    HTML -> peeks $ otherRouteResponse [] viewAsset (api, assetId $ assetRow $ slotAsset as'')

postAsset :: ActionRoute (API, Id Asset)
postAsset = multipartAction $ action POST (pathAPI </> pathId) $ \(api, ai) -> withAuth $ do
  asset <- getAsset PermissionEDIT ai
  r <- assetIsReplaced (slotAsset asset)
  when r $ result $
    response conflict409 [] ("This file has already been replaced." :: T.Text)
  processAsset api $ AssetTargetAsset asset

viewAssetEdit :: ActionRoute (Id Asset)
viewAssetEdit = action GET (pathHTML >/> pathId </< "edit") $ \ai -> withAuth $ do
  asset <- getAsset PermissionEDIT ai
  peeks $ blankForm . htmlAssetEdit (AssetTargetAsset asset)

createAsset :: ActionRoute (API, Id Volume)
createAsset = multipartAction $ action POST (pathAPI </> pathId </< "asset") $ \(api, vi) -> withAuth $ do
  v <- getVolume PermissionEDIT vi
  processAsset api $ AssetTargetVolume v

viewAssetCreate :: ActionRoute (Id Volume)
viewAssetCreate = action GET (pathHTML >/> pathId </< "asset") $ \vi -> withAuth $ do
  v <- getVolume PermissionEDIT vi
  peeks $ blankForm . htmlAssetEdit (AssetTargetVolume v)

createSlotAsset :: ActionRoute (API, Id Slot)
createSlotAsset = multipartAction $ action POST (pathAPI </> pathSlotId </< "asset") $ \(api, si) -> withAuth $ do
  v <- getSlot PermissionEDIT Nothing si
  processAsset api $ AssetTargetSlot v

viewSlotAssetCreate :: ActionRoute (Id Slot)
viewSlotAssetCreate = action GET (pathHTML >/> pathSlotId </< "asset") $ \si -> withAuth $ do
  s <- getSlot PermissionEDIT Nothing si
  peeks $ blankForm . htmlAssetEdit (AssetTargetSlot s)

deleteAsset :: ActionRoute (API, Id Asset)
deleteAsset = action DELETE (pathAPI </> pathId) $ \(api, ai) -> withAuth $ do
  guardVerfHeader
  asset <- getAsset PermissionEDIT ai
  let asset' = asset{ assetSlot = Nothing }
  _ <- changeAssetSlot asset'
  case api of
    JSON -> return $ okResponse [] $ JSON.recordEncoding $ assetSlotJSON asset'
    HTML -> peeks $ otherRouteResponse [] viewAsset (api, assetId $ assetRow $ slotAsset asset')

downloadAsset :: ActionRoute (Id Asset, Segment)
downloadAsset = action GET (pathId </> pathSegment </< "download") $ \(ai, seg) -> withAuth $ do
  a <- getAsset PermissionPUBLIC ai
  inline <- peeks $ lookupQueryParameters "inline"
  serveAssetSegment (null inline) $ newAssetSegment a seg Nothing

thumbAsset :: ActionRoute (Id Asset, Segment)
thumbAsset = action GET (pathId </> pathSegment </< "thumb") $ \(ai, seg) -> withAuth $ do
  a <- getAsset PermissionPUBLIC ai
  let as = assetSegmentInterp 0.25 $ newAssetSegment a seg Nothing
  if formatIsImage (view as) && assetBacked (view as) && dataPermission as > PermissionNONE
    then peeks $ otherRouteResponse [] downloadAsset (view as, assetSegment as)
    else peeks $ otherRouteResponse [] formatIcon (view as)
