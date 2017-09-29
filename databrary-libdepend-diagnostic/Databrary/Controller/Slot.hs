{-# LANGUAGE OverloadedStrings #-}
module Databrary.Controller.Slot
  ( getSlot
  , viewSlot
  , slotDownloadName
  , thumbSlot
  ) where

import Control.Monad (when, mfilter)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.Maybe (isJust)
import Data.Monoid ((<>))
import qualified Data.Text as T
import Network.HTTP.Types.Status (movedPermanently301)
import qualified Network.Wai as Wai

import Databrary.Has (view, peeks)
import qualified Databrary.JSON as JSON
import Databrary.Model.Id
import Databrary.Model.Permission
import Databrary.Model.Volume
import Databrary.Model.Container
import Databrary.Model.Slot
import Databrary.Model.Asset
import Databrary.Model.AssetSlot
import Databrary.Model.AssetSegment
import Databrary.Model.Excerpt
import Databrary.Model.Record
import Databrary.Model.RecordSlot
import Databrary.Model.Tag
import Databrary.Model.Comment
import Databrary.Store.Filename
import Databrary.HTTP.Path.Parser
import Databrary.Action
import Databrary.Controller.Paths
import Databrary.Controller.Permission
import Databrary.Controller.Angular
import Databrary.Controller.Container
import Databrary.Controller.Web
import {-# SOURCE #-} Databrary.Controller.AssetSegment

getSlot :: Permission -> Maybe (Id Volume) -> Id Slot -> ActionM Slot
getSlot p mv i =
  checkPermission p =<< maybeAction . maybe id (\v -> mfilter $ (v ==) . view) mv =<< lookupSlot i

slotJSONField :: Slot -> BS.ByteString -> Maybe BS.ByteString -> ActionM (Maybe JSON.Encoding)
slotJSONField o "assets" _ =
  Just . JSON.mapRecords assetSlotJSON <$> lookupSlotAssets o
slotJSONField o "records" _ =
  Just . JSON.mapRecords (\r -> recordSlotJSON r JSON..<> "record" JSON..=: recordJSON (slotRecord r)) <$> lookupSlotRecords o
slotJSONField o "tags" n = do
  tc <- lookupSlotTagCoverage o (maybe 64 fst $ BSC.readInt =<< n)
  return $ Just $ JSON.objectEncoding $ JSON.recordMap $ map tagCoverageJSON tc
slotJSONField o "comments" n = do
  c <- lookupSlotComments o (maybe 64 fst $ BSC.readInt =<< n)
  return $ Just $ JSON.mapRecords commentJSON c
slotJSONField o "excerpts" _ =
  Just . JSON.mapObjects (\e -> excerptJSON e <> "asset" JSON..= (view e :: Id Asset)) <$> lookupSlotExcerpts o
slotJSONField o "filename" _ =
  return $ Just $ JSON.toEncoding $ makeFilename $ slotDownloadName o
slotJSONField _ _ _ = return Nothing

slotJSONQuery :: Slot -> JSON.Query -> ActionM (JSON.Record (Id Container) JSON.Series)
slotJSONQuery o q = (slotJSON o JSON..<>) <$> JSON.jsonQuery (slotJSONField o) q

slotDownloadName :: Slot -> [T.Text]
slotDownloadName s = containerDownloadName (slotContainer s)

viewSlot :: ActionRoute (API, (Maybe (Id Volume), Id Slot))
viewSlot = action GET (pathAPI </> pathMaybe pathId </> pathSlotId) $ \(api, (vi, i)) -> withAuth $ do
  when (api == HTML && isJust vi) angular
  c <- getSlot PermissionPUBLIC vi i
  case api of
    JSON -> okResponse [] <$> (slotJSONQuery c =<< peeks Wai.queryString)
    HTML
      | isJust vi -> return $ okResponse [] $ BSC.pack $ show $ containerId $ containerRow $ slotContainer c -- TODO
      | otherwise -> peeks $ redirectRouteResponse movedPermanently301 [] viewSlot (api, (Just (view c), slotId c))

thumbSlot :: ActionRoute (Maybe (Id Volume), Id Slot)
thumbSlot = action GET (pathMaybe pathId </> pathSlotId </< "thumb") $ \(vi, i) -> withAuth $ do
  s <- getSlot PermissionPUBLIC vi i
  e <- lookupSlotSegmentThumb s
  maybe
    (peeks $ otherRouteResponse [] webFile (Just $ staticPath ["images", "draft.png"]))
    (\as -> peeks $ otherRouteResponse [] downloadAssetSegment (slotId $ view as, assetId $ assetRow $ view as))
    e
