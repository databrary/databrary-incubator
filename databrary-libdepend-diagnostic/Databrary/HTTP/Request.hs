{-# LANGUAGE OverloadedStrings #-}
module Databrary.HTTP.Request
  ( Wai.Request
  , MonadHasRequest
  , lookupRequestHeader
  , lookupRequestHeaders
  , lookupQueryParameters
  , boolParameterValue
  , boolQueryParameter
  , requestHost
  , requestURI
  ) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Network.HTTP.Types (HeaderName)
import Network.URI (URI(..), URIAuth(..))
import qualified Network.Wai as Wai

import Databrary.Has (MonadHas)

type MonadHasRequest c m = MonadHas Wai.Request c m

lookupRequestHeader :: HeaderName -> Wai.Request -> Maybe BS.ByteString
lookupRequestHeader h = lookup h . Wai.requestHeaders

lookupRequestHeaders :: HeaderName -> Wai.Request -> [BS.ByteString]
lookupRequestHeaders h = map snd . filter ((h ==) . fst) . Wai.requestHeaders

lookupQueryParameters :: BS.ByteString -> Wai.Request -> [Maybe BS.ByteString]
lookupQueryParameters q = map snd . filter ((q ==) . fst) . Wai.queryString

boolValue :: BS.ByteString -> Bool
boolValue "0" = False
boolValue "false" = False
boolValue "off" = False
boolValue "" = False
boolValue _ = True

boolParameterValue :: Maybe BS.ByteString -> Bool
boolParameterValue = all boolValue

boolQueryParameter :: BS.ByteString -> Wai.Request -> Bool
boolQueryParameter q = any boolParameterValue . lookupQueryParameters q

defaultHost :: BS.ByteString
defaultHost = "databrary.org"

requestHost :: Wai.Request -> BS.ByteString
requestHost req =
  (if Wai.isSecure req then "https://" else "http://")
  <> fromMaybe "databrary.org" (Wai.requestHeaderHost req)

requestURI :: Wai.Request -> URI
requestURI req = URI
  { uriScheme = if Wai.isSecure req then "https:" else "http:"
  , uriAuthority = Just URIAuth
    { uriUserInfo = ""
    , uriRegName = BSC.unpack $ fromMaybe defaultHost $ Wai.requestHeaderHost req
    , uriPort = ""
    }
  , uriPath = BSC.unpack $ Wai.rawPathInfo req
  , uriQuery = BSC.unpack $ Wai.rawQueryString req
  , uriFragment = ""
  }
