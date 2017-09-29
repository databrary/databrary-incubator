{-# LANGUAGE OverloadedStrings #-}
module Databrary.HTTP.Cookie
  ( getSignedCookie
  , setSignedCookie
  , clearCookie
  ) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Lazy as BSL
import Network.HTTP.Types (Header, hCookie)
import qualified Network.Wai as Wai
import qualified Web.Cookie as Cook

import Databrary.Ops
import Databrary.Has
import Databrary.Service.Crypto
import Databrary.Service.Types
import Databrary.Model.Time
import Databrary.HTTP.Request

getCookies :: Request -> Cook.Cookies
getCookies = maybe [] Cook.parseCookies . lookupRequestHeader hCookie

getSignedCookie :: (MonadHas Secret c m, MonadHasRequest c m) => BS.ByteString -> m (Maybe BS.ByteString)
getSignedCookie c = flatMapM unSign . lookup c =<< peeks getCookies

setSignedCookie :: (MonadSign c m, MonadHasRequest c m) => BS.ByteString -> BS.ByteString -> Timestamp -> m Header
setSignedCookie c val ex = do
  val' <- sign val
  sec <- peeks Wai.isSecure
  return ("set-cookie", BSL.toStrict $ BSB.toLazyByteString $ Cook.renderSetCookie $ Cook.def
    { Cook.setCookieName = c
    , Cook.setCookieValue = val'
    , Cook.setCookiePath = Just "/"
    , Cook.setCookieExpires = Just ex
    , Cook.setCookieSecure = sec
    , Cook.setCookieHttpOnly = True
    })

clearCookie :: BS.ByteString -> Header
clearCookie c = ("set-cookie", BSL.toStrict $ BSB.toLazyByteString $ Cook.renderSetCookie $ Cook.def
  { Cook.setCookieName = c
  , Cook.setCookiePath = Just "/"
  })
