{-# LANGUAGE OverloadedStrings #-}
module Databrary.Controller.Tag
  ( queryTags
  , postTag
  , deleteTag
  ) where

import Control.Monad (unless)
import qualified Data.Text as T
import Network.HTTP.Types (conflict409)
import qualified Web.Route.Invertible as R

import Databrary.Has
import Databrary.Ops
import qualified Databrary.JSON as JSON
import Databrary.Model.Permission
import Databrary.Model.Id
import Databrary.Model.Container
import Databrary.Model.Slot
import Databrary.Model.Tag
import Databrary.Model.Notification.Types
import Databrary.Solr.Tag
import Databrary.HTTP.Form.Deform
import Databrary.HTTP.Path.Parser
import Databrary.Action.Run
import Databrary.Action
import Databrary.Controller.Paths
import Databrary.Controller.Form
import Databrary.Controller.Permission
import Databrary.Controller.Slot
import Databrary.Controller.Notification

_tagNameForm :: DeformActionM f TagName
_tagNameForm = deformMaybe' "Invalid tag name." . validateTag =<< deform

queryTags :: ActionRoute (Maybe TagName)
queryTags = action GET (pathJSON >/> "tags" >/> pathMaybe R.parameter) $ \t -> withoutAuth $
  okResponse [] . JSON.toEncoding <$> termTags t 16

tagResponse :: API -> TagUse -> ActionM Response
tagResponse JSON t = okResponse [] . JSON.recordEncoding . tagCoverageJSON <$> lookupTagCoverage (useTag t) (containerSlot $ slotContainer $ tagSlot t)
tagResponse HTML t = peeks $ otherRouteResponse [] viewSlot (HTML, (Just (view t), slotId (tagSlot t)))

postTag :: ActionRoute (API, Id Slot, TagId)
postTag = action POST (pathAPI </>> pathSlotId </> pathTagId) $ \(api, si, TagId kw tn) -> withAuth $ do
  guardVerfHeader
  u <- authAccount
  s <- getSlot (if kw then PermissionEDIT else PermissionSHARED) Nothing si
  t <- addTag tn
  let tu = TagUse t kw u s
  r <- addTagUse tu
  unless r $ result $
    response conflict409 [] ("The requested tag overlaps your existing tag." :: T.Text)
  top <- containerIsVolumeTop (slotContainer s)
  createVolumeNotification (view tu) $ \n -> (n NoticeTagVolume)
    { notificationContainerId = top ?!> view tu
    , notificationSegment = Just $ view tu
    , notificationTag = Just $ view tu
    }
  tagResponse api tu

deleteTag :: ActionRoute (API, Id Slot, TagId)
deleteTag = action DELETE (pathAPI </>> pathSlotId </> pathTagId) $ \(api, si, TagId kw tn) -> withAuth $ do
  guardVerfHeader
  u <- authAccount
  s <- getSlot (if kw then PermissionEDIT else PermissionSHARED) Nothing si
  t <- maybeAction =<< lookupTag tn
  let tu = TagUse t kw u s
  _r <- removeTagUse tu
  tagResponse api tu
