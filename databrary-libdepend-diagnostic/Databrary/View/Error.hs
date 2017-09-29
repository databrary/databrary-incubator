{-# LANGUAGE OverloadedStrings #-}
module Databrary.View.Error
  ( htmlNotFound
  , htmlForbidden
  ) where

import Control.Monad (void)
import qualified Text.Blaze.Html5 as H

import Databrary.Action.Types
import Databrary.View.Template

htmlNotFound :: RequestContext -> H.Html
htmlNotFound req = htmlTemplate req (Just "Not found") $ \_ -> do
  void $ "The resource you requested may no longer be available."

htmlForbidden :: RequestContext -> H.Html
htmlForbidden req = htmlTemplate req (Just "Access denied") $ \_ -> do
  void $ "You do not have access to the requested resource."
