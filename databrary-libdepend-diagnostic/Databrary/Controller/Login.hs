{-# LANGUAGE OverloadedStrings #-}
module Databrary.Controller.Login
  ( checkPassword
  , loginAccount
  , viewLogin
  , postLogin
  , postLogout
  , viewUser
  , postUser
  ) where

import Control.Applicative ((<|>))
import Control.Monad (when, unless)
import Control.Monad.Trans.Class (lift)
import qualified Crypto.BCrypt as BCrypt
import qualified Data.ByteString as BS
import Data.Function (on)
import Data.Maybe (fromMaybe)
import qualified Network.Wai as Wai

import Databrary.Ops
import Databrary.Has
import qualified Databrary.JSON as JSON
import Databrary.Model.Id.Types
import Databrary.Model.Party
import Databrary.Model.Identity
import Databrary.Model.Permission
import Databrary.Model.Notification
import Databrary.Model.Token
import Databrary.HTTP.Cookie
import Databrary.HTTP.Form.Deform
import Databrary.HTTP.Path.Parser
import Databrary.Action
import Databrary.Controller.Paths
import Databrary.Controller.Form
import Databrary.Controller.Angular
import Databrary.Controller.Notification
import Databrary.View.Login

import {-# SOURCE #-} Databrary.Controller.Root
import {-# SOURCE #-} Databrary.Controller.Party

loginAccount :: API -> SiteAuth -> Bool -> ActionM Response
loginAccount api auth su = do
  sess <- createSession auth su
  let Token (Id tok) ex = view sess
  cook <- setSignedCookie "session" tok ex
  case api of
    JSON -> return $ okResponse [cook] $ JSON.recordEncoding $ identityJSON (Identified sess)
    HTML -> peeks $ otherRouteResponse [cook] viewParty (HTML, TargetProfile)

viewLogin :: ActionRoute ()
viewLogin = action GET ("user" >/> "login") $ \() -> withAuth $ do
  angular
  maybeIdentity
    (peeks $ blankForm . htmlLogin)
    (const $ peeks $ otherRouteResponse [] viewParty (HTML, TargetProfile))

checkPassword :: BS.ByteString -> SiteAuth -> Bool
checkPassword p = any (`BCrypt.validatePassword` p) . accountPasswd

postLogin :: ActionRoute API
postLogin = action POST (pathAPI </< "user" </< "login") $ \api -> withoutAuth $ do
  (Just auth, su) <- runForm (api == HTML ?> htmlLogin) $ do
    email <- "email" .:> emailTextForm
    password <- "password" .:> deform
    superuser <- "superuser" .:> deform
    auth <- lift $ lookupSiteAuthByEmail True email
    let p = view <$> auth
        su = superuser && any ((PermissionADMIN ==) . accessMember) auth
    attempts <- lift $ maybe (return 0) recentAccountLogins p
    let pass = checkPassword password `any` auth
        block = attempts > 4
    lift $ auditAccountLogin pass (fromMaybe nobodyParty p) email
    when block $ "email" .:> deformError "Too many login attempts. Try again later."
    unless pass $ "password" .:> deformError "Incorrect email address or password. Both are case-sensitive, and institutional addresses are preferred."
    return (auth, su)
  loginAccount api auth su

postLogout :: ActionRoute API
postLogout = action POST (pathAPI </< "user" </< "logout") $ \api -> withAuth $ do
  _ <- maybeIdentity (return False) removeSession
  case api of
    JSON -> return $ okResponse [cook] $ JSON.recordEncoding $ identityJSON NotIdentified
    HTML -> peeks $ otherRouteResponse [cook] viewRoot HTML
  where cook = clearCookie "session"

userJSONField :: BS.ByteString -> Maybe BS.ByteString -> ActionM (Maybe JSON.Encoding)
userJSONField "notifications" _ = Just . JSON.toEncoding <$> countUserNotifications
userJSONField _ _ = return Nothing

viewUser :: ActionRoute ()
viewUser = action GET (pathJSON </< "user") $ \() -> withAuth $ do
  i <- peeks identityJSON
  q <- JSON.jsonQuery userJSONField =<< peeks Wai.queryString
  return $ okResponse [] (i JSON..<> q)

postUser :: ActionRoute API
postUser = action POST (pathAPI </< "user") $ \api -> withAuth $ do
  auth <- peek
  let acct = siteAccount auth
  auth' <- runForm (api == HTML ?> htmlUserForm acct) $ do
    csrfForm
    "auth" .:> (deformGuard "Incorrect password" . (`checkPassword` auth) =<< deform)
    email <- "email" .:> deformNonEmpty emailTextForm
    passwd <- "password" .:> deformNonEmpty (passwordForm acct)
    let acct' = acct
          { accountEmail = fromMaybe (accountEmail acct) email
          , accountParty = (accountParty acct){ partyAccount = Just acct' }
          }
    return auth
      { siteAccount = acct'
      , accountPasswd = passwd <|> accountPasswd auth
      }
  changeAccount auth'
  when (on (/=) (accountEmail . siteAccount) auth' auth || on (/=) accountPasswd auth' auth) $
    createNotification (blankNotification acct NoticeAccountChange) -- use old acct (email)
      { notificationParty = Just $ partyRow $ accountParty acct
      , notificationDelivered = DeliveryAsync -- force immediate delivery
      }
  case api of
    JSON -> return $ okResponse [] $ JSON.recordEncoding $ partyJSON $ accountParty $ siteAccount auth'
    HTML -> peeks $ otherRouteResponse [] viewParty (api, TargetProfile)
