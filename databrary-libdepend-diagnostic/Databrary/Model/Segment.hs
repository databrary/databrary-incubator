{-# LANGUAGE OverloadedStrings, DataKinds, DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Databrary.Model.Segment
  ( Segment(..)
  , lowerBound, upperBound
  , showSegmentWith
  , segmentLength
  , fullSegment
  , emptySegment
  , segmentFull
  , segmentEmpty
  , segmentContains
  , segmentOverlaps
  , segmentIntersect
  , segmentInterp
  , segmentJSON
  , segmentSetDuration
  ) where

import Control.Applicative ((<|>), optional)
import Control.Monad (guard, liftM2)
import Data.Maybe (fromMaybe, isNothing)
import qualified Data.Text as T
import Data.Typeable (Typeable)
import Database.PostgreSQL.Typed.Types (PGType, PGParameter(..), PGColumn(..))
import Database.PostgreSQL.Typed.Array (PGArrayType)
import qualified Database.PostgreSQL.Typed.Range as Range
import qualified Text.ParserCombinators.ReadP as RP
import qualified Text.ParserCombinators.ReadPrec as RP (lift, readPrec_to_P, minPrec)
import Text.Read (readMaybe, readPrec)
import qualified Web.Route.Invertible as R

import Databrary.Ops
import qualified Databrary.JSON as JSON
import Databrary.Model.Offset

lowerBound, upperBound :: Range.Range a -> Maybe a
lowerBound = Range.bound . Range.lowerBound
upperBound = Range.bound . Range.upperBound

newtype Segment = Segment { segmentRange :: Range.Range Offset } deriving (Eq, Ord, Typeable)

instance PGType "segment"
instance Range.PGRangeType "segment" "interval"
instance PGType "segment[]"
instance PGArrayType "segment[]" "segment"

instance PGParameter "segment" Segment where
  pgEncode t (Segment r) = pgEncode t r
instance PGColumn "segment" Segment where
  pgDecode t = Segment . pgDecode t

segmentLength :: Segment -> Maybe Offset
segmentLength (Segment r) =
  liftM2 (-) (upperBound r) (lowerBound r)

showSegmentWith :: (Offset -> ShowS) -> Segment -> ShowS
showSegmentWith _ (Segment Range.Empty) = showString "empty"
showSegmentWith sf (Segment r)
  | Just x <- Range.getPoint r = sf x
  | otherwise =
  maybe id (((if Range.lowerClosed r then id else showChar '(') .) . sf) (lowerBound r)
  . showChar ',' . maybe id sf (upperBound r)
  . (if Range.upperClosed r then showChar ']' else id)

instance Show Segment where
  showsPrec = showSegmentWith . showsPrec

readP :: Read a => RP.ReadP a
readP = RP.readPrec_to_P readPrec RP.minPrec

instance Read Segment where
  readPrec = RP.lift $ Segment <$> re RP.+++ rf RP.+++ rr where
    re = do
      RP.optional (RP.string "empty")
      return Range.Empty
    rf = do
      _ <- RP.char '-'
      return Range.full
    rr :: RP.ReadP (Range.Range Offset)
    rr = do
      lb <- optional $ ('[' ==) <$> RP.satisfy (`elem` ['(','['])
      l <- optional readP
      (guard (isNothing lb) >> Range.point <$> maybeA l) RP.+++ do
        _ <- if isNothing lb && isNothing l then RP.char ',' else RP.satisfy (`elem` [',','-'])
        u <- optional readP
        ub <- optional $ ('[' ==) <$> RP.satisfy (`elem` [')',']'])
        return $ Range.range (mb True lb l) (mb False ub u)
    -- more liberal than Range.makeBound:
    mb :: Bool -> Maybe Bool -> Maybe Offset -> Range.Bound Offset
    mb d = maybe Range.Unbounded . Range.Bounded . fromMaybe d

instance JSON.ToJSON Segment where
  toJSON (Segment r)
    | Range.isEmpty r = JSON.Null
    | Just o <- Range.getPoint r = JSON.toJSON o
    | otherwise = JSON.toJSON $ map Range.bound [Range.lowerBound r, Range.upperBound r]

instance JSON.FromJSON Segment where
  parseJSON (JSON.String s) = maybe (fail "Invalid segment string") return $ readMaybe $ T.unpack s
  parseJSON j = do
    a <- JSON.parseJSON j <|> return <$> JSON.parseJSON j
    Segment <$> case a of
      [] -> return Range.empty
      [p] -> return $ maybe Range.empty Range.point p
      [l, u] -> return $ Range.normal l u
      _ -> fail "Segment array too long"

instance R.Parameter T.Text Segment where
  renderParameter s = T.pack $ showSegmentWith (shows . offsetMillis) s ""

fullSegment :: Segment
fullSegment = Segment Range.full

emptySegment :: Segment
emptySegment = Segment Range.empty

segmentFull :: Segment -> Bool
segmentFull = Range.isFull . segmentRange

segmentEmpty :: Segment -> Bool
segmentEmpty = Range.isEmpty . segmentRange

segmentContains :: Segment -> Segment -> Bool
segmentContains (Segment a) (Segment b) = a Range.@> b

segmentOverlaps :: Segment -> Segment -> Bool
segmentOverlaps (Segment a) (Segment b) = Range.overlaps a b

segmentIntersect :: Segment -> Segment -> Segment
segmentIntersect (Segment a) (Segment b) = Segment (Range.intersect a b)

segmentInterp :: Float -> Segment -> Segment
segmentInterp f (Segment r)
  | Just u <- upperBound r = Segment (Range.point (l + realToFrac f * (u - l)))
  | otherwise = Segment (Range.point 0)
  where l = fromMaybe 0 $ lowerBound r

segmentJSON :: JSON.ToObject o => Segment -> o
segmentJSON s = "segment" JSON..=? (s <!? segmentFull s)

segmentSetDuration :: Offset -> Segment -> Segment
segmentSetDuration o (Segment (Range.Range lb@(Range.Lower (Range.Bounded _ l)) (Range.Upper ub))) =
  Segment (Range.Range lb (Range.Upper (Range.Bounded (Range.boundClosed ub) (l + o))))
segmentSetDuration _ s = s
