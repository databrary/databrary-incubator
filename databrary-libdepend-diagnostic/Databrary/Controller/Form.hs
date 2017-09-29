{-# LANGUAGE OverloadedStrings #-}
module Databrary.Controller.Form
  ( FormData
  , DeformActionM
  , runFormFiles
  , runForm
  , blankForm

  , emailTextForm
  , passwordForm
  , paginateForm
  , csrfForm
  ) where

import Control.Applicative ((<|>))
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ask)
import Control.Monad.Trans.Class (lift)
import qualified Crypto.BCrypt as BCrypt
import qualified Data.Aeson as JSON
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.Char (toLower)
import Data.Monoid ((<>))
import qualified Data.Text.Encoding as TE
import Data.Word (Word64)
import Network.HTTP.Types (badRequest400)
import qualified Text.Blaze.Html5 as Html
import qualified Text.Regex.Posix as Regex

import Databrary.Has
import Databrary.Model.Paginate
import Databrary.Model.Party
import Databrary.Model.Identity
import Databrary.Service.Passwd
import Databrary.HTTP.Parse (FileContent)
import Databrary.HTTP.Form (FormData)
import Databrary.HTTP.Form.Deform
import Databrary.HTTP.Form.View (runFormView, blankFormView)
import Databrary.HTTP.Form.Errors (FormErrors)
import Databrary.Action.Response
import Databrary.Action.Types
import Databrary.Action.Form (getFormData)
import Databrary.Controller.Permission (checkVerfHeader)
import Databrary.View.Form (FormHtml)

type DeformActionM f a = DeformT f ActionM a

jsonFormErrors :: FormErrors -> Response
jsonFormErrors = response badRequest400 [] . JSON.toEncoding

htmlFormErrors :: (FormErrors -> Html.Html) -> FormErrors -> Response
htmlFormErrors f = response badRequest400 [] . f

handleForm :: (FormErrors -> Response) -> Either FormErrors a -> ActionM a
handleForm re = either (result . re) return

handleFormErrors :: Maybe (FormErrors -> Html.Html) -> Either FormErrors a -> ActionM a
handleFormErrors = handleForm . maybe jsonFormErrors htmlFormErrors

runFormWith :: FormData f -> Maybe (RequestContext -> FormHtml f) -> DeformActionM f a -> ActionM a
runFormWith fd mf fa = do
  req <- ask
  let fv hv = runFormView (hv req) fd
  handleFormErrors (fv <$> mf) =<< runDeform fa fd

runFormFiles :: FileContent f => [(BS.ByteString, Word64)] -> Maybe (RequestContext -> FormHtml f) -> DeformActionM f a -> ActionM a
runFormFiles fl mf fa = do
  fd <- getFormData fl
  runFormWith fd mf fa

runForm :: Maybe (RequestContext -> FormHtml ()) -> DeformActionM () a -> ActionM a
runForm = runFormFiles []

blankForm :: FormHtml f -> Response
blankForm = okResponse [] . blankFormView

emailRegex :: Regex.Regex
emailRegex = Regex.makeRegexOpts Regex.compIgnoreCase Regex.blankExecOpt
  ("^[-a-z0-9!#$%&'*+/=?^_`{|}~.]*@[a-z0-9][a-z0-9\\.-]*[a-z0-9]\\.[a-z][a-z\\.]*[a-z]$" :: String)

emailTextForm :: DeformActionM f BS.ByteString
emailTextForm = do
  e <- deformCheck "Invalid email address" (Regex.matchTest emailRegex) =<< deform
  return $ maybe e (uncurry ((. BSC.map toLower) . (<>)) . flip BS.splitAt e) $ BSC.elemIndex '@' e

passwordForm :: Account -> DeformActionM f BS.ByteString
passwordForm acct = do
  p <- "once" .:> do
    p <- deform
    deformGuard "Password too short. Must be 7 characters." (7 <= BS.length p)
    c <- lift $ focusIO $ passwdCheck p (accountEmail acct) (TE.encodeUtf8 $ partyName $ partyRow $ accountParty acct)
    mapM_ (deformError . ("Insecure password: " <>) . TE.decodeLatin1) c
    return p
  "again" .:> do
    a <- deform
    deformGuard "Passwords do not match." (a == p)
  pw <- liftIO $ BCrypt.hashPasswordUsingPolicy passwordPolicy p
  deformMaybe' "Error processing password." pw

paginateForm :: DeformActionM f Paginate
paginateForm = Paginate
  <$> get "offset" paginateOffset
  <*> get "limit" paginateLimit
  where get t f = t .:> (deformCheck ("invalid " <> t) (\i -> i >= f minBound && i <= f maxBound) =<< deform) <|> return (f def)

csrfForm :: DeformActionM f ()
csrfForm = do
  r <- lift checkVerfHeader
  unless r $ do
    verf <- lift $ peeks identityVerf
    "csverf" .:> maybe
      (deformError "You must be logged in to perform this request.")
      (\v -> deformGuard "Invalid form verifier. Please logout, reload, and try again." . (v ==) =<< deform)
      verf
