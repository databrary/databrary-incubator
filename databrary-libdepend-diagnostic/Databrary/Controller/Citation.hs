{-# LANGUAGE OverloadedStrings #-}
module Databrary.Controller.Citation
  ( getCitation
  ) where

import Databrary.Has (focusIO)
import qualified Databrary.JSON as JSON
import Databrary.HTTP.Form.Deform
import Databrary.HTTP.Path.Parser
import Databrary.Action
import Databrary.Controller.Form
import Databrary.Model.Citation.CrossRef

getCitation :: ActionRoute ()
getCitation = action GET (pathJSON </< "cite") $ \() -> withoutAuth $ do
  url <- runForm Nothing $ "url" .:> deform
  cite <- maybeAction =<< focusIO (lookupCitation url)
  return $ okResponse [] $ JSON.toEncoding cite
