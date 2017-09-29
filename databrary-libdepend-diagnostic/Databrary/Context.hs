{-# LANGUAGE TemplateHaskell #-}
module Databrary.Context
  ( Context(..)
  , ContextM
  , runContextM
  , BackgroundContext(..)
  , BackgroundContextM
  , withBackgroundContextM
  ) where

import Control.Monad.Trans.Reader (ReaderT(..), withReaderT)
import Control.Monad.Trans.Resource (InternalState, runResourceT, withInternalState)
import Data.Time (getCurrentTime)

import Databrary.Has
import Databrary.Model.Time
import Databrary.Model.Id.Types
import Databrary.Model.Identity.Types
import Databrary.Model.Party.Types
import Databrary.Model.Permission.Types
import Databrary.Service.Types
import Databrary.Service.DB

data Context = Context
  { contextService :: !Service
  , contextTimestamp :: !Timestamp
  , contextResourceState :: !InternalState
  , contextDB :: !DBConn
  }

makeHasRec ''Context ['contextService, 'contextTimestamp, 'contextResourceState, 'contextDB]

type ContextM a = ReaderT Context IO a

runContextM :: ContextM a -> Service -> IO a
runContextM f rc = do
  t <- getCurrentTime
  runResourceT $ withInternalState $ \is ->
    withDB (serviceDB rc) $
      runReaderT f . Context rc t is

newtype BackgroundContext = BackgroundContext { backgroundContext :: Context }

makeHasRec ''BackgroundContext ['backgroundContext]

instance Has Identity BackgroundContext where
  view _ = NotIdentified
instance Has SiteAuth BackgroundContext where
  view _ = view NotIdentified
instance Has Party BackgroundContext where
  view _ = view NotIdentified
instance Has (Id Party) BackgroundContext where
  view _ = view NotIdentified
instance Has Access BackgroundContext where
  view _ = view NotIdentified

type BackgroundContextM a = ReaderT BackgroundContext IO a

withBackgroundContextM :: BackgroundContextM a -> ContextM a
withBackgroundContextM = withReaderT BackgroundContext
