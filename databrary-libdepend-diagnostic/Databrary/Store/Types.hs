module Databrary.Store.Types
  ( Transcoder(..)
  , Storage(..)
  , MonadStorage
  ) where

import Control.Monad.IO.Class (MonadIO)

import Databrary.Has (MonadHas)
import Databrary.Files

data Transcoder = Transcoder
  { transcoderCmd :: !FilePath
  , transcoderArgs :: ![String]
  }

data Storage = Storage
  { storageMaster :: !RawFilePath
  , storageFallback :: !(Maybe RawFilePath)
  , storageTemp :: !RawFilePath
  , storageUpload :: !RawFilePath
  , storageCache :: !(Maybe RawFilePath)
  , storageStage :: !(Maybe RawFilePath)
  , storageTranscoder :: !(Maybe Transcoder)
  }

type MonadStorage c m = (MonadHas Storage c m, MonadIO m)
