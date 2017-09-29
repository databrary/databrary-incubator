{-# LANGUAGE OverloadedStrings #-}
module Databrary.Static.Service
  ( Static(..)
  , initStatic
  ) where

import qualified Crypto.Hash.Algorithms as Hash
import Crypto.MAC.HMAC (HMAC, hmac)
import qualified Data.ByteString as BS
import Data.Maybe (fromMaybe)
import qualified Network.HTTP.Client as HC
import Network.HTTP.Types (methodPost, hContentType)

import qualified Databrary.Store.Config as C

data Static = Static
  { staticAuthorizeAddr :: !BS.ByteString
  , staticAssistAddr :: !BS.ByteString
  , staticInvestigator :: !(Maybe HC.Request)
  , staticKey :: !(BS.ByteString -> HMAC Hash.SHA256)
  }

initStatic :: C.Config -> IO Static
initStatic conf = do
  fillin <- mapM HC.parseUrl $ conf C.! "fillin"
  return $ Static
    { staticAuthorizeAddr = conf C.! "authorize"
    , staticAssistAddr = conf C.! "assist"
    , staticInvestigator = fmap (\f -> f
      { HC.method = methodPost
      , HC.requestHeaders = (hContentType, "application/x-www-form-urlencoded") : HC.requestHeaders f
      , HC.cookieJar = Nothing
      , HC.redirectCount = 0
      }) fillin
    , staticKey = hmac $ fromMaybe ("databrary" :: BS.ByteString) $ conf C.! "key"
    }
