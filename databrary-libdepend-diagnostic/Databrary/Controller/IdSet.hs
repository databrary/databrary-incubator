{-# LANGUAGE OverloadedStrings #-}
module Databrary.Controller.IdSet
  ( IdSet
  , idSetIsFull
  , requestIdSet
  , idSetQuery
  ) where

import qualified Data.ByteString.Char8 as BSC
import Data.List (foldl')
import Data.Maybe (mapMaybe)
import qualified Data.RangeSet.List as RS
import qualified Data.RangeSet.Parse as RS
import Network.HTTP.Types (Query)
import qualified Network.Wai as Wai

import Databrary.Model.Id.Types

type IdSet a = RS.RSet (Id a)

idSetIsFull :: (Eq (IdType a), Bounded (IdType a)) => IdSet a -> Bool
idSetIsFull = (==) RS.full

requestIdSet :: (Read (IdType a), Ord (IdType a), Enum (IdType a), Bounded (IdType a)) => Wai.Request -> IdSet a
requestIdSet = pe . mapMaybe ie . Wai.queryString where
  pe [] = RS.full
  pe (h:l) = foldl' ae (either RS.complement id h) l
  ae s (Right i) = s `RS.union` i
  ae s (Left e) = s `RS.difference` e
  ie (k, Nothing) = Right <$> ps k
  ie (k, Just v)
    | k `BSC.isPrefixOf` "include" = Right <$> ps v
    | k `BSC.isPrefixOf` "exclude" = Left <$> ps v
  ie _ = Nothing
  ps = RS.parseRangeSet . BSC.unpack

idSetQuery :: (Show (IdType a), Ord (IdType a), Enum (IdType a), Bounded (IdType a)) => IdSet a -> Query
idSetQuery s
  | idSetIsFull s = []
  | RS.findMin s == minBound = vs "exclude" $ RS.complement s
  | otherwise = vs "include" s where
  vs ie = return . (,) ie . Just . BSC.pack . RS.showRangeSet
