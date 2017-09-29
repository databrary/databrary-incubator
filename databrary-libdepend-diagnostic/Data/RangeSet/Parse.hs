{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data.RangeSet.Parse
  ( showRangeSet
  , parseRangeSet
  ) where

import Control.Applicative (optional)
import Data.Maybe (fromMaybe)
import qualified Data.RangeSet.List as R
import qualified Text.ParserCombinators.ReadP as RP
import qualified Text.ParserCombinators.ReadPrec as RP (lift, readPrec_to_P, minPrec)
import Text.Read (readMaybe, readPrec)

newtype RangeList a = RangeList { unRangeList :: [(a,a)] } deriving (Monoid)

rangeSetList :: R.RSet a -> RangeList a
rangeSetList = RangeList . R.toRangeList

rangeListSet :: (Ord a, Enum a) => RangeList a -> R.RSet a
rangeListSet = R.fromRangeList . unRangeList

instance (Show a, Eq a, Bounded a) => Show (RangeList a) where
  showsPrec _ = sl . unRangeList where
    sl [] = id
    sl [r] = sr r
    sl (r:l) = sr r . showChar ',' . sl l
    sr (a,b)
      | a == b = shows a
      | otherwise = (if a == minBound then id else shows a)
        . showChar '-' .
        (if b == maxBound then id else shows b)

showRangeSet :: (Show a, Eq a, Bounded a) => R.RSet a -> String
showRangeSet = show . rangeSetList

readP :: Read a => RP.ReadP a
readP = RP.readPrec_to_P readPrec RP.minPrec

instance (Read a, Bounded a) => Read (RangeList a) where
  readPrec = RP.lift $ RangeList <$> RP.sepBy rr (RP.char ',') where
    ru = do
      _ <- RP.char '-'
      RP.option maxBound readP
    rr = do
      l <- optional readP
      let lb = fromMaybe minBound l
      ub <- maybe ru (`RP.option` ru) l
      return (lb, ub)

parseRangeSet :: (Ord a, Enum a, Bounded a, Read a) => String -> Maybe (R.RSet a)
parseRangeSet = fmap rangeListSet . readMaybe
