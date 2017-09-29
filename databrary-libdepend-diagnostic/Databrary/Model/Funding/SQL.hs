{-# LANGUAGE TemplateHaskell #-}
module Databrary.Model.Funding.SQL
  ( makeFunding -- TODO: move to Types
  ) where

import Data.Maybe (fromMaybe)
import qualified Data.Text as T

import Databrary.Model.Funding.Types

makeFunding :: [Maybe T.Text] -> Funder -> Funding
makeFunding a f = Funding f (map (fromMaybe (error "NULL funding.award")) a)
