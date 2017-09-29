module Databrary.Ingest.Service
  ( IngestStatus(..)
  , Ingest(..)
  , initIngest
  ) where

import Control.Concurrent (ThreadId)
import Control.Concurrent.MVar (MVar, newMVar)
import Data.Int (Int32)
import qualified Data.Text as T

data IngestStatus
  = IngestInactive
  | IngestActive ThreadId
  | IngestCompleted [Int32]
  | IngestFailed [T.Text]

newtype Ingest = Ingest
  { ingestStatus :: MVar IngestStatus
  }

initIngest :: IO Ingest
initIngest = Ingest <$> newMVar IngestInactive

