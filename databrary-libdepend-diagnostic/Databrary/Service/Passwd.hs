{-# LANGUAGE ForeignFunctionInterface, OverloadedStrings, TemplateHaskell #-}
module Databrary.Service.Passwd
  ( passwordPolicy
  , passwdCheck
  , Passwd
  , initPasswd
  ) where

import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import qualified Crypto.BCrypt as BCrypt
import qualified Data.ByteString as BS
import Foreign.C.String (CString)
import Foreign.Ptr (nullPtr)

import Databrary.Ops

passwordPolicy :: BCrypt.HashingPolicy
passwordPolicy = BCrypt.HashingPolicy
  { BCrypt.preferredHashAlgorithm = "$2b$"
  , BCrypt.preferredHashCost = 12
  }

foreign import ccall unsafe "crack.h FascistCheckUser"
  fascistCheckUser :: CString -> CString -> CString -> CString -> IO CString

newtype Passwd = Passwd { _passwdLock :: MVar () }

initPasswd :: IO Passwd
initPasswd = Passwd <$> newMVar ()

passwdCheck :: BS.ByteString -> BS.ByteString -> BS.ByteString -> Passwd -> IO (Maybe BS.ByteString)
passwdCheck passwd user name (Passwd lock) =
  withMVar lock $ \() ->
    BS.useAsCString passwd $ \p ->
      BS.useAsCString user $ \u ->
        BS.useAsCString name $ \n -> do
          r <- fascistCheckUser p nullPtr u n
          r /= nullPtr ?$> BS.packCString r
