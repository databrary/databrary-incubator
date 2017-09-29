{-# LANGUAGE OverloadedStrings, TupleSections #-}
module Databrary.HTTP.Client
  ( HTTPClient
  , initHTTPClient
  , checkContentOk
  , CookiesT
  , runCookiesT
  , withCookies
  , withResponseCookies
  , requestAcceptContent
  , httpParse
  , httpMaybe
  , httpRequestJSON
  ) where

import Control.Arrow ((&&&))
import Control.Exception (SomeException, toException)
import Control.Exception.Lifted (handle)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Trans.State.Strict (StateT(..), evalStateT)
import qualified Data.Aeson as JSON
import qualified Data.Attoparsec.ByteString as P
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.CaseInsensitive as CI
import Data.Foldable (fold)
import Data.Function (on)
import Data.Monoid ((<>))
import qualified Network.HTTP.Client as HC
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types (ResponseHeaders, hAccept, hContentType, Status, statusIsSuccessful)

import Databrary.Has

type HTTPClient = HC.Manager

initHTTPClient :: IO HTTPClient
initHTTPClient = HC.newManager tlsManagerSettings
  { HC.managerConnCount = 4
  , HC.managerIdleConnectionCount = 8
  }

type CookiesT m a = StateT HC.CookieJar m a

runCookiesT :: Monad m => CookiesT m a -> m a
runCookiesT f = evalStateT f mempty

withCookies :: (MonadIO m, MonadHas HTTPClient c m) => (HC.Request -> HC.Manager -> IO (HC.Response a)) -> HC.Request -> CookiesT m (HC.Response a)
withCookies f r = StateT $ \c -> focusIO $ \m ->
  (id &&& HC.responseCookieJar) <$> f r{ HC.cookieJar = HC.cookieJar r <> Just c } m

withResponseCookies :: (MonadIO m, MonadHas HTTPClient c m) => HC.Request -> (HC.Response HC.BodyReader -> IO a) -> CookiesT m a
withResponseCookies q f = StateT $ \c -> focusIO $ \m ->
  HC.withResponse q{ HC.cookieJar = HC.cookieJar q <> Just c } m $ \r -> (, HC.responseCookieJar r) <$> f r

contentTypeEq :: BS.ByteString -> BS.ByteString -> Bool
contentTypeEq = (==) `on` f where
  f s
    | Just i <- BSC.elemIndex ';' s = BS.take i s
    | otherwise = s

checkContentOk :: BS.ByteString -> Status -> ResponseHeaders -> HC.CookieJar -> Maybe SomeException
checkContentOk ct s h cj
  | not $ statusIsSuccessful s = Just $ toException $ HC.StatusCodeException s h cj
  | not $ any (contentTypeEq ct) ht = Just $ toException $ HC.InvalidHeader $ CI.original hContentType <> ": " <> fold ht
  | otherwise = Nothing
  where ht = lookup hContentType h

requestAcceptContent :: BS.ByteString -> HC.Request -> HC.Request
requestAcceptContent ct req = req
  { HC.requestHeaders = (hAccept, ct) : HC.requestHeaders req
  , HC.checkStatus = checkContentOk ct
  }

httpParse :: P.Parser a -> HC.Response HC.BodyReader -> IO (P.Result a)
httpParse p r = P.parseWith (HC.responseBody r) p BS.empty

httpMaybe :: MonadBaseControl IO m => m (Maybe a) -> m (Maybe a)
httpMaybe = handle (return . fail . (show :: HC.HttpException -> String))

httpRequestJSON :: HC.Request -> HTTPClient -> IO (Maybe JSON.Value)
httpRequestJSON r m = httpMaybe $
  HC.withResponse (requestAcceptContent "application/json" r) m (fmap P.maybeResult . httpParse JSON.json)
