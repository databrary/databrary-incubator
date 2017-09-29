{-# LANGUAGE OverloadedStrings #-}
module Databrary.Service.Crypto
  ( signature
  , MonadSign
  , sign
  , unSign
  ) where

import Control.Monad (mfilter)
import Control.Monad.IO.Class (MonadIO)
import qualified Crypto.Hash as Hash
import qualified Crypto.MAC.HMAC as HMAC
import qualified Data.ByteArray as BA
import Data.ByteArray.Encoding (convertToBase, convertFromBase, Base(Base64URLUnpadded))
import qualified Data.ByteString as BS
import Data.Monoid ((<>))

import Databrary.Ops
import Databrary.Has
import Databrary.Service.Types
import Databrary.Service.Entropy

hmac :: BS.ByteString -> BS.ByteString -> BS.ByteString
hmac key = (convertToBase Base64URLUnpadded :: HMAC.HMAC Hash.Skein256_224 -> BS.ByteString) . HMAC.hmac key

hmacLength :: Int
hmacLength = BS.length $ hmac "" ""

signature :: BS.ByteString -> Secret -> BS.ByteString
signature msg (Secret secret) = hmac secret msg

nonceBytes, nonceLength :: Int
nonceBytes = 6
nonceLength = BA.length $ encodeNonce $ BA.zero nonceBytes -- 8

encodeNonce :: BS.ByteString -> BS.ByteString
encodeNonce = convertToBase Base64URLUnpadded

type MonadSign c m = (MonadIO m, MonadHas Entropy c m, MonadHas Secret c m)

sign :: MonadSign c m => BS.ByteString -> m BS.ByteString
sign msg = do
  nonce <- focusIO $ entropyBytes nonceBytes
  sig <- peeks $ signature (msg <> nonce)
  return $ sig <> encodeNonce nonce <> msg

unSign :: (MonadHas Secret c m) => BS.ByteString -> m (Maybe BS.ByteString)
unSign sigmsg = do
  sig' <- mapM (peeks . signature . (msg <>)) nonce
  return $ msg <$ mfilter (BA.constEq sig) sig'
  where
  (sig, noncemsg) = BS.splitAt hmacLength sigmsg
  (nonce64, msg) = BS.splitAt nonceLength noncemsg
  nonce = rightJust $ convertFromBase Base64URLUnpadded nonce64
