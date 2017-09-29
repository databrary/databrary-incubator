{-# LANGUAGE TemplateHaskell, TypeFamilies, OverloadedStrings #-}
module Databrary.Model.Comment.Types
  ( Comment(..)
  , CommentRow(..)
  , makeComment
  , makeCommentRow
  ) where

import Data.Maybe (fromMaybe)
import qualified Data.Text as T

import Databrary.Has (makeHasRec)
import Databrary.Model.Kind
import Databrary.Model.Time
import Databrary.Model.Id.Types
import Databrary.Model.Party.Types
import Databrary.Model.Slot.Types
import Databrary.Model.Segment
import Databrary.Model.Container.Types

type instance IdType Comment = Int32

data Comment = Comment
  { commentId :: Id Comment
  , commentWho :: Account
  , commentSlot :: Slot
  , commentTime :: Timestamp
  , commentText :: T.Text
  , commentParents :: [Id Comment]
  }

instance Kinded Comment where
  kindOf _ = "comment"

makeHasRec ''Comment ['commentId, 'commentWho, 'commentSlot, 'commentTime]

data CommentRow = CommentRow
  { commentRowId :: Id Comment
  , commentRowWhoId :: Id Party
  , commentRowSlotId :: SlotId
  , commentRowTime :: Timestamp
  , commentRowText :: T.Text
  }

makeComment :: Id Comment -> Segment -> Timestamp -> T.Text -> [Maybe (Id Comment)] -> Account -> Container -> Comment
makeComment i s t x p w c = Comment i w (Slot c s) t x (map (fromMaybe (error "NULL comment thread")) p)

makeCommentRow :: Id Comment -> Id Container -> Segment -> Id Party -> Timestamp -> T.Text -> CommentRow
makeCommentRow i c s w t x = CommentRow i w (SlotId c s) t x
