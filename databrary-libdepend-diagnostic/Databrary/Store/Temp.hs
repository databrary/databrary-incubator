{-# LANGUAGE OverloadedStrings, TupleSections #-}
module Databrary.Store.Temp
  ( TempFile(..)
  , makeTempFileAs
  , makeTempFile
  , releaseTempFile
  , renameTempFile
  ) where

import Control.Exception (bracket)
import Control.Monad (void)
import Control.Monad.Trans.Resource (InternalState, runInternalState, ReleaseKey, allocate, release, unprotect)
import System.IO (Handle, hClose)
import System.Posix.FilePath (RawFilePath)
import System.Posix.Files.ByteString (removeLink, rename)
import System.Posix.Temp.ByteString (mkstemp)

import Databrary.Has (peeks, focusIO)
import Databrary.Store.Types
import Databrary.Action.Types

data TempFile = TempFile
  { tempFileRelease :: ReleaseKey
  , tempFilePath :: RawFilePath
  }

makeTempFileAs :: RawFilePath -> (Handle -> IO ()) -> InternalState -> IO TempFile
makeTempFileAs d g rs = bracket
  (runInternalState (allocate (mkstemp d) (removeLink . fst)) rs)
  (hClose . snd . snd)
  (\(k, (f, h)) -> TempFile k f <$ g h)

makeTempFile :: (Handle -> IO ()) -> ActionM TempFile
makeTempFile f = do
  tmp <- peeks storageTemp
  focusIO $ makeTempFileAs tmp f

releaseTempFile :: TempFile -> InternalState -> IO ()
releaseTempFile = runInternalState . release . tempFileRelease

renameTempFile :: TempFile -> RawFilePath -> InternalState -> IO ()
renameTempFile (TempFile k f) t rs = do
  rename f t
  void $ runInternalState (unprotect k) rs
