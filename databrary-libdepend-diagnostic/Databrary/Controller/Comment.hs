{-# LANGUAGE OverloadedStrings #-}
module Databrary.Controller.Comment
  ( postComment
  ) where

import Control.Monad (forM_, when)
import Control.Monad.Trans.Class (lift)
import Data.Function (on)

import Databrary.Ops
import Databrary.Has
import qualified Databrary.JSON as JSON
import Databrary.Model.Permission
import Databrary.Model.Id
import Databrary.Model.Container
import Databrary.Model.Slot
import Databrary.Model.Notification.Types
import Databrary.Model.Party.Types
import Databrary.Model.Comment
import Databrary.HTTP.Form.Deform
import Databrary.HTTP.Path.Parser
import Databrary.Action
import Databrary.Controller.Paths
import Databrary.Controller.Permission
import Databrary.Controller.Form
import Databrary.Controller.Slot
import Databrary.Controller.Notification
import Databrary.View.Comment

postComment :: ActionRoute (API, Id Slot)
postComment = action POST (pathAPI </> pathSlotId </< "comment") $ \(api, si) -> withAuth $ do
  u <- authAccount
  s <- getSlot PermissionSHARED Nothing si
  (c, p) <- runForm (api == HTML ?> htmlCommentForm s) $ do
    csrfForm
    text <- "text" .:> (deformRequired =<< deform)
    parent <- "parent" .:> deformNonEmpty (deformMaybe' "comment not found" =<< lift . lookupComment =<< deform)
    return ((blankComment u s)
      { commentText = text
      , commentParents = maybe [] (return . commentId) parent
      }, parent)
  c' <- addComment c
  top <- containerIsVolumeTop (slotContainer s)
  forM_ p $ \r -> when (on (/=) (partyId . partyRow . accountParty) (commentWho r) u) $
    createNotification (blankNotification (commentWho r) NoticeCommentReply)
      { notificationContainerId = top ?!> view c'
      , notificationSegment = Just $ view c'
      , notificationCommentId = Just $ view c'
      }
  createVolumeNotification (view c') $ \n -> (n NoticeCommentVolume)
    { notificationContainerId = top ?!> view c'
    , notificationSegment = Just $ view c'
    , notificationCommentId = Just $ view c'
    }
  case api of
    JSON -> return $ okResponse [] $ JSON.recordEncoding $ commentJSON c'
    HTML -> peeks $ otherRouteResponse [] viewSlot (api, (Just (view c'), slotId (commentSlot c')))
