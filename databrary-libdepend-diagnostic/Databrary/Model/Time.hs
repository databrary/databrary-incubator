{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Databrary.Model.Time
  ( Date
  , Timestamp
  , dateYear
  , MaskedDate
  , maskDateIf
  , maskedYear
  ) where

import qualified Data.Aeson as JSON
import Data.Fixed (Fixed(..))
import Data.Time (Day(..), UTCTime(..), DiffTime, toGregorian, fromGregorian)
import Data.Time.Format (FormatTime(..), formatTime, dateFmt)
import Language.Haskell.TH.Lift (deriveLiftMany)

import Databrary.Has (Has(..))

type Date = Day
type Timestamp = UTCTime

deriveLiftMany [''Fixed, ''DiffTime, ''Day, ''UTCTime]

instance Has Day Timestamp where
  view = utctDay

data MaskedDate
  = MaskedDate !Int
  | UnmaskedDate !Date

dateYear :: Date -> Int
dateYear d = fromInteger y where (y,_,_) = toGregorian d

maskDate :: Date -> MaskedDate
maskDate = MaskedDate . dateYear

maskDateIf :: Bool -> Date -> MaskedDate
maskDateIf True = maskDate
maskDateIf False = UnmaskedDate

maskedYear :: MaskedDate -> Int
maskedYear (MaskedDate y) = y
maskedYear (UnmaskedDate d) = dateYear d

instance FormatTime MaskedDate where
  formatCharacter 'D' = Just (\locale _ -> formatTime locale "%m/%d/%y")
  formatCharacter 'F' = Just (\locale _ -> formatTime locale "%Y-%m-%d")
  formatCharacter 'x' = Just (\locale _ -> formatTime locale (dateFmt locale))
  formatCharacter c = f <$> formatCharacter c where
    f g l o (UnmaskedDate d) = g l o d
    f g l o (MaskedDate y)
      | c `elem` "YyCGgf" = r
      | otherwise = map (const 'X') r
      where r = g l o $ fromGregorian (toInteger y) 11 21

instance JSON.ToJSON MaskedDate where
  toJSON (MaskedDate y) = JSON.toJSON y
  toJSON (UnmaskedDate d) = JSON.toJSON d
