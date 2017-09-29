{-# LANGUAGE CPP #-}
module Databrary.Web.Service
  ( Web
  , initWeb
  ) where

#ifdef DEVEL
import Control.Concurrent.MVar (newMVar)
#endif

import Databrary.Web.Types
import Databrary.Web.Info

initWeb :: IO Web
initWeb =
  fmap Web $
#ifdef DEVEL
    newMVar =<<
#endif
    loadWebFileMap
