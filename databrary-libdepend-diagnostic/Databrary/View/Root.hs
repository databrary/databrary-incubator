{-# LANGUAGE OverloadedStrings #-}
module Databrary.View.Root
  ( htmlRoot
  , htmlDown
  ) where

import qualified Data.Text as T
import qualified Text.Blaze.Html5 as H

import Databrary.Has (view)
import Databrary.Model.Identity
import Databrary.Action.Types
import Databrary.Action.Route
import Databrary.Controller.Paths
import Databrary.Controller.Login
import Databrary.Controller.Volume
import Databrary.Controller.Party
import Databrary.Controller.Activity
import Databrary.View.Template
import Databrary.View.Html

htmlRoot :: RequestContext -> H.Html
htmlRoot req = htmlTemplate req Nothing $ \js -> do
  H.ul $ do
    H.li $ foldIdentity
      (H.a H.! actionLink viewLogin () js $ "login")
      (\_ -> H.a H.! actionLink viewParty (HTML, TargetProfile) js $ "profile")
      (view req)
    H.li $ H.a H.! actionLink viewSiteActivity HTML js $ "activity"
    H.li $ H.a H.! actionLink queryVolumes HTML js $ "volumes"
    H.li $ H.a H.! actionLink queryParties HTML js $ "parties"
  return ()

htmlDown :: T.Text -> RequestContext -> H.Html
htmlDown msg req = htmlTemplate req (Just "Status") $ \_ -> do
  H.div $ do
    H.p $ H.preEscapedText msg
    H.p "Many services are unavailable, including all file uploads and downloads. Minimal account and authorization management is available."
  return ()
