{-# LANGUAGE CPP #-}
module Databrary.Web.Types
  ( WebFileInfo(..)
  , WebFileMap
  , Web(..)
  , WebGeneratorM
  , WebGenerator
  ) where

#ifdef DEVEL
import Control.Concurrent.MVar (MVar)
#endif
import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Trans.State.Strict (StateT)
import Crypto.Hash (Digest, MD5)
import qualified Data.ByteString as BS
import qualified Data.HashMap.Strict as HM

import Databrary.Model.Time
import Databrary.Web

data WebFileInfo = WebFileInfo
  { webFileFormat :: BS.ByteString
  , webFileHash :: !(Digest MD5)
  , webFileTimestamp :: !Timestamp
  }

type WebFileMap = HM.HashMap WebFilePath WebFileInfo

data Web = Web
  { webCache ::
#ifdef DEVEL
    MVar
#endif
      WebFileMap
  }

type WebGeneratorM a = ExceptT String (StateT WebFileMap IO) a
type WebGenerator = (WebFilePath, Maybe WebFileInfo) -> WebGeneratorM Bool

