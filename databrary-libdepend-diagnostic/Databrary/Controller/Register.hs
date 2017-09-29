{-# LANGUAGE OverloadedStrings #-}
module Databrary.Controller.Register
  ( viewPasswordReset
  , postPasswordReset
  , viewRegister
  , postRegister
  , resendInvestigator
  ) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BSB
import Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE

import Databrary.Ops
import Databrary.Has
import Databrary.Service.Mail
import Databrary.Static.Fillin
import Databrary.Model.Permission
import Databrary.Model.Id
import Databrary.Model.Party
import Databrary.Model.Identity
import Databrary.Model.Token
import Databrary.HTTP.Form.Deform
import Databrary.HTTP.Path.Parser
import Databrary.Action
import Databrary.Controller.Paths
import Databrary.Controller.Form
import Databrary.Controller.Permission
import Databrary.Controller.Party
import Databrary.Controller.Token
import Databrary.Controller.Angular
import Databrary.View.Register

resetPasswordMail :: Either BS.ByteString SiteAuth -> T.Text -> (Maybe TL.Text -> TL.Text) -> ActionM ()
resetPasswordMail (Left email) subj body =
  sendMail [Left email] [] subj (body Nothing)
resetPasswordMail (Right auth) subj body = do
  tok <- loginTokenId =<< createLoginToken auth True
  req <- peek
  sendMail [Right $ view auth] [] subj
    (body $ Just $ TLE.decodeLatin1 $ BSB.toLazyByteString $ actionURL (Just req) viewLoginToken (HTML, tok) [])

viewRegister :: ActionRoute ()
viewRegister = action GET (pathHTML </< "user" </< "register") $ \() -> withAuth $ do
  angular
  maybeIdentity
    (peeks $ blankForm . htmlRegister)
    (\_ -> peeks $ otherRouteResponse [] viewParty (HTML, TargetProfile))

postRegister :: ActionRoute API
postRegister = action POST (pathAPI </< "user" </< "register") $ \api -> withoutAuth $ do
  reg <- runForm (api == HTML ?> htmlRegister) $ do
    name <- "sortname" .:> (deformRequired =<< deform)
    prename <- "prename" .:> deformNonEmpty deform
    email <- "email" .:> emailTextForm
    affiliation <- "affiliation" .:> deformNonEmpty deform
    _ <- "agreement" .:> (deformCheck "You must consent to the user agreement." id =<< deform)
    let p = blankParty
          { partyRow = (partyRow blankParty)
            { partySortName = name
            , partyPreName = prename
            , partyAffiliation = affiliation
            }
          , partyAccount = Just a
          }
        a = Account
          { accountParty = p
          , accountEmail = email
          }
    return a
  auth <- maybe (SiteAuth <$> addAccount reg <*- Nothing <*- mempty) return =<< lookupSiteAuthByEmail False (accountEmail reg)
  resetPasswordMail (Right auth)
    "Databrary account created"
    $ \(Just url) ->
      "Thank you for registering with Databrary. Please use this link to complete your registration:\n\n"
      <> url <> "\n\n\
      \By clicking the above link, you also indicate that you have read and understand the Databrary Access agreement, which you can download here: http://databrary.org/policies/agreement.pdf\n\n\
      \Once you've validated your e-mail, you will be able to request authorization to be granted full access to Databrary.\n"
  focusIO $ staticSendInvestigator (view auth)
  return $ okResponse [] $ "Your confirmation email has been sent to '" <> accountEmail reg <> "'."

resendInvestigator :: ActionRoute (Id Party)
resendInvestigator = action POST (pathHTML >/> pathId </< "investigator") $ \i -> withAuth $ do
  checkMemberADMIN
  p <- getParty (Just PermissionREAD) (TargetParty i)
  focusIO $ staticSendInvestigator p
  return $ okResponse [] ("sent" :: String)

viewPasswordReset :: ActionRoute ()
viewPasswordReset = action GET (pathHTML </< "user" </< "password") $ \() -> withoutAuth $ do
  angular
  peeks $ blankForm . htmlPasswordReset

postPasswordReset :: ActionRoute API
postPasswordReset = action POST (pathAPI </< "user" </< "password") $ \api -> withoutAuth $ do
  email <- runForm (api == HTML ?> htmlPasswordReset) $ do
    "email" .:> emailTextForm
  auth <- lookupPasswordResetAccount email
  resetPasswordMail (maybe (Left email) Right auth)
    "Databrary password reset" $
    ("Someone (hopefully you) has requested to reset the password for the Databrary account associated with this email address. If you did not request this, let us know (by replying to this message) or simply ignore it.\n\n" <>)
    . maybe
      "Unfortunately, no Databrary account was found for this email address. You can try again with a different email address, or reply to this email for assistance.\n"
      ("Otherwise, you may use this link to reset your Databrary password:\n\n" <>)
  return $ okResponse [] $ "Your password reset information has been sent to '" <> email <> "'."

