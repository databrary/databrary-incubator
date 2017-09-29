{-# LANGUAGE OverloadedStrings #-}
module Databrary.View.Authorize
  ( authorizeSiteTitle
  , htmlAuthorizeForm
  ) where

import qualified Data.ByteString.Char8 as BSC
import qualified Data.Text as T

import qualified Databrary.Store.Config as C
import Databrary.Service.Messages
import Databrary.Action
import Databrary.View.Form
import Databrary.Model.Party
import Databrary.Model.Permission
import Databrary.Model.Authorize
import Databrary.Controller.Paths

import {-# SOURCE #-} Databrary.Controller.Authorize

authorizeSiteTitle :: Permission -> Messages -> T.Text
authorizeSiteTitle site = getMessage $ C.Path ["auth", "site", BSC.pack (show site), "title"]

htmlAuthorizeForm :: Authorize -> RequestContext -> FormHtml f
htmlAuthorizeForm a = htmlForm
  ("Authorize " `T.append` partyName (partyRow child))
  postAuthorize (HTML, TargetParty $ partyId $ partyRow parent, AuthorizeTarget False $ partyId $ partyRow child)
  (do
    field "site" $ inputEnum True $ Just $ accessSite a
    field "member" $ inputEnum True $ Just $ accessMember a
    field "expires" $ inputText $ Just $ show $ authorizeExpires a
    field "delete" $ inputCheckbox False)
  (const mempty)
  where
  Authorization
    { authorizeChild = child
    , authorizeParent = parent
    } = authorization a

