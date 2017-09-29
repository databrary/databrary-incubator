{-# LANGUAGE DeriveDataTypeable #-}
module Databrary.HTTP.Method.Map
  ( MethodMap(..)
  , singleton
  , insert
  , lookup
  ) where

import Prelude hiding (lookup)

import Control.Applicative ((<|>))
import Control.Exception (Exception, throw)
import qualified Data.Map.Strict as M
import Data.Typeable (Typeable)
import Network.HTTP.Types.Method (Method, StdMethod(HEAD, GET), parseMethod)

import Databrary.Ops

data MethodMapException
  = MethodMapConflict StdMethod
  deriving (Typeable, Show)

instance Exception MethodMapException

newtype MethodMap a = MethodMap (M.Map StdMethod a)

conflict :: StdMethod -> a -> a -> a
conflict k _ _ = throw $ MethodMapConflict k

singleton :: StdMethod -> a -> MethodMap a
singleton m a = MethodMap $ M.singleton m a

insert :: StdMethod -> a -> MethodMap a -> MethodMap a
insert m a (MethodMap mm) = MethodMap $ M.insertWithKey conflict m a mm

lookup :: Method -> MethodMap a -> Either [StdMethod] a
lookup m (MethodMap mm) = maybe (Left (M.keys mm)) Right $
  lm =<< rightJust (parseMethod m) where
  lm HEAD = M.lookup HEAD mm <|> lm GET
  lm k = M.lookup k mm

instance Monoid (MethodMap a) where
  mempty = MethodMap mempty
  mappend (MethodMap a) (MethodMap b) = MethodMap $ M.unionWithKey conflict a b
