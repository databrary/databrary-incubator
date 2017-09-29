{-# LANGUAGE OverloadedStrings #-}
module Databrary.View.Comment
  ( htmlCommentForm
  ) where

import qualified Data.Text as T

import Databrary.Action
import Databrary.Model.Slot
import Databrary.View.Form

import {-# SOURCE #-} Databrary.Controller.Comment

htmlCommentForm :: Slot -> RequestContext -> FormHtml f
htmlCommentForm slot = htmlForm "Comment" postComment (HTML, slotId slot)
  (field "text" $ inputText (Nothing :: Maybe T.Text))
  (const mempty)
