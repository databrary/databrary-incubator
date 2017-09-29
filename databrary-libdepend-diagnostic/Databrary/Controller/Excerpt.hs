{-# LANGUAGE OverloadedStrings #-}
module Databrary.Controller.Excerpt
  ( postExcerpt
  , deleteExcerpt
  ) where

import Control.Monad (unless, when)
import Data.Maybe (isNothing)
import qualified Data.Text as T
import Network.HTTP.Types (conflict409)

import Databrary.Has
import qualified Databrary.JSON as JSON
import Databrary.Model.Id
import Databrary.Model.Permission
import Databrary.Model.Slot
import Databrary.Model.Asset
import Databrary.Model.AssetSegment
import Databrary.Model.Excerpt
import Databrary.Model.Notification.Types
import Databrary.HTTP.Form.Deform
import Databrary.HTTP.Path.Parser
import Databrary.Action
import Databrary.Controller.Permission
import Databrary.Controller.Paths
import Databrary.Controller.Form
import Databrary.Controller.Notification
import Databrary.Controller.AssetSegment

pathExcerpt :: PathParser (Id Slot, Id Asset)
pathExcerpt = pathJSON >/> pathSlotId </> pathId </< "excerpt"

postExcerpt :: ActionRoute (Id Slot, Id Asset)
postExcerpt = action POST pathExcerpt $ \(si, ai) -> withAuth $ do
  as <- getAssetSegment PermissionEDIT Nothing si ai
  e <- runForm Nothing $ do
    csrfForm
    Excerpt as <$> ("release" .:> deformNonEmpty deform)
  r <- changeExcerpt e
  unless r $ result $
    response conflict409 [] ("The requested excerpt overlaps an existing excerpt." :: T.Text)
  let notice t = createVolumeNotification (view e) $ \n -> (n t)
        { notificationContainerId = Just $ view e
        , notificationSegment = Just $ view e
        , notificationAssetId = Just $ view e
        , notificationRelease = excerptRelease e
        }
  when (isNothing $ assetExcerpt as) $
    notice NoticeExcerptVolume
  when (any (view as <) $ excerptRelease e) $
    notice NoticeReleaseExcerpt
  return $ okResponse [] $ JSON.objectEncoding $ assetSegmentJSON (if r then as{ assetExcerpt = Just e } else as)

deleteExcerpt :: ActionRoute (Id Slot, Id Asset)
deleteExcerpt = action DELETE pathExcerpt $ \(si, ai) -> withAuth $ do
  guardVerfHeader
  as <- getAssetSegment PermissionEDIT Nothing si ai
  r <- removeExcerpt as
  return $ okResponse [] $ JSON.objectEncoding $ assetSegmentJSON (if r then as{ assetExcerpt = Nothing } else as)
