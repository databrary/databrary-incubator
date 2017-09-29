{-# LANGUAGE TupleSections #-}
module Databrary.Ingest.Action
  ( getIngestStatus
  , runIngest
  , abortIngest
  , clearIngest
  ) where

import Control.Arrow (left)
import Control.Concurrent (killThread)
import Control.Concurrent.MVar (readMVar, swapMVar, withMVar, modifyMVar, modifyMVar_)
import Control.Monad (join, void)
import Data.Int (Int32)
import qualified Data.Text as T

import Databrary.Has (view, focusIO)
import Databrary.Action.Types
import Databrary.Action.Run
import Databrary.Ingest.Service

getIngestStatus :: Ingest -> IO IngestStatus
getIngestStatus = readMVar . ingestStatus

runIngest :: ActionM (Either [T.Text] [Int32]) -> ActionM Bool
runIngest r = focusIO $ \c -> let v = ingestStatus (view c) in
  modifyMVar v $ \s ->
    case s of
      IngestActive _ -> return (s, False)
      _ -> (, True) . IngestActive <$> forkAction r c
        (void . swapMVar v . either IngestFailed IngestCompleted . join . left (return . T.pack . show))

abortIngest :: Ingest -> IO ()
abortIngest Ingest{ ingestStatus = v } = withMVar v abt where
  abt (IngestActive t) = killThread t
  abt _ = return ()

clearIngest :: Ingest -> IO ()
clearIngest Ingest{ ingestStatus = v } = modifyMVar_ v clr where
  clr s@(IngestActive _) = return s
  clr _ = return IngestInactive
