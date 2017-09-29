{-# LANGUAGE GeneralizedNewtypeDeriving, TypeFamilies, TemplateHaskell #-}
module Databrary.Action.Types
  ( RequestContext(..)
  , ActionM(..)
  , runActionM
  ) where

import Control.Applicative (Alternative)
import Control.Monad (MonadPlus)
import Control.Monad.Base (MonadBase)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader, ReaderT(..))
import Control.Monad.Trans.Control (MonadBaseControl(..))
import Control.Monad.Trans.Resource (MonadThrow, MonadResource(..), runInternalState)

import Databrary.Has
import Databrary.Model.Identity
import Databrary.HTTP.Request
import Databrary.Context

data RequestContext = RequestContext
  { requestContext :: !Context
  , contextRequest :: !Request
  , requestIdentity :: !Identity
  }

-- Example of a invoking Template Haskell code generation. This invokes the makeHasRec code generator, 
--   which generates convenience functions to access the fields of a RequestContext
makeHasRec ''RequestContext ['requestContext, 'contextRequest, 'requestIdentity]

newtype ActionM a = ActionM { unActionM :: ReaderT RequestContext IO a }
  deriving (Functor, Applicative, Alternative, Monad, MonadPlus, MonadIO, MonadBase IO, MonadThrow, MonadReader RequestContext)

{-# INLINE runActionM #-}
runActionM :: ActionM a -> RequestContext -> IO a
runActionM (ActionM (ReaderT f)) = f

instance MonadResource ActionM where
  liftResourceT = focusIO . runInternalState

instance MonadBaseControl IO ActionM where
  type StM ActionM a = a
  liftBaseWith f = ActionM $ liftBaseWith $ \r -> f (r . unActionM)
  restoreM = ActionM . restoreM
