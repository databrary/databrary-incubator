module Databrary.Model.Periodic
  ( Period(..)
  ) where

import Control.Exception (Exception(..), asyncExceptionToException, asyncExceptionFromException)
import Data.Typeable (Typeable)

data Period
  = PeriodDaily
  | PeriodWeekly
  deriving (Typeable, Eq, Ord, Enum, Show)

instance Exception Period where
  toException = asyncExceptionToException
  fromException = asyncExceptionFromException

