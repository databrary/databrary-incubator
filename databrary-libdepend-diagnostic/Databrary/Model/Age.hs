{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Databrary.Model.Age
  ( Age(..)
  , age
  , yearsAge
  , ageTime
  , ageLimit
  ) where

import Data.Time (diffDays, DiffTime, secondsToDiffTime)

import qualified Databrary.JSON as JSON
import Databrary.Model.Time

newtype Age = Age { ageDays :: Int } deriving (Eq, Ord, Num)

instance JSON.ToJSON Age where
  toJSON (Age days) = JSON.Number $ fromIntegral days

age :: Date -> Date -> Age
age b d = Age $ fromInteger $ diffDays d b

yearsAge :: Real a => a -> Age
yearsAge y = Age $ ceiling $ (365.24219 :: Double) * realToFrac y

ageTime :: Age -> DiffTime
ageTime (Age n) = secondsToDiffTime $ 86400 * fromIntegral n

ageLimit :: Age
ageLimit = yearsAge (90 :: Double)
