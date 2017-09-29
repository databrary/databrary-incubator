{-# LANGUAGE CPP, OverloadedStrings #-}
module Databrary.Controller.Token
  ( lookupPasswordResetAccount
  , viewLoginToken
  , postPasswordToken
  ) where

#if !defined(DEVEL) && !defined(SANDBOX)
import Control.Monad (mfilter)
#endif
import Control.Monad (when, unless)
import qualified Data.ByteString as BS
import Data.Maybe (isNothing, isJust)

import Databrary.Ops
import Databrary.Has
import qualified Databrary.JSON as JSON
import Databrary.Model.Id
import Databrary.Model.Token
import Databrary.Model.Party
#if !defined(DEVEL) && !defined(SANDBOX)
import Databrary.Model.Permission
#endif
import Databrary.Model.Notification.Types
import Databrary.HTTP.Path.Parser
import Databrary.Action.Run
import Databrary.Action
import Databrary.Controller.Paths
import Databrary.Controller.Form
import Databrary.Controller.Login
import Databrary.Controller.Angular
import Databrary.Controller.Notification
import Databrary.View.Token

lookupPasswordResetAccount :: BS.ByteString -> ActionM (Maybe SiteAuth)
lookupPasswordResetAccount email =
#if !defined(DEVEL) && !defined(SANDBOX)
  mfilter ((PermissionADMIN >) . accessMember) <$>
#endif
  lookupSiteAuthByEmail True email

viewLoginToken :: ActionRoute (API, Id LoginToken)
viewLoginToken = action GET (pathAPI </> pathId) $ \(api, ti) -> withoutAuth $ do
  when (api == HTML) angular
  tok <- maybeAction =<< lookupLoginToken ti
  if loginPasswordToken tok
    then case api of
      JSON -> return $ okResponse [] $ JSON.recordEncoding $ JSON.Record ti $
        "reset" JSON..= isJust (accountPasswd (view tok))
      HTML -> peeks $ blankForm . htmlPasswordToken ti
    else do
      _ <- removeLoginToken tok
      loginAccount api (view tok) False

postPasswordToken :: ActionRoute (API, Id LoginToken)
postPasswordToken = action POST (pathAPI </> pathId) $ \(api, ti) -> withoutAuth $ do
  tok <- maybeAction =<< lookupLoginToken ti
  unless (loginPasswordToken tok) $ result =<< peeks notFoundResponse
  let auth = view tok
  pw <- runForm (api == HTML ?> htmlPasswordToken ti) $
    passwordForm (siteAccount auth)
  changeAccount auth{ accountPasswd = Just pw } -- or should this be withAuth?
  _ <- removeLoginToken tok
  unless (isNothing $ accountPasswd auth) $
    createNotification (blankNotification (siteAccount auth) NoticeAccountChange)
      { notificationParty = Just $ partyRow $ accountParty $ siteAccount auth }
  loginAccount api (view tok) False
