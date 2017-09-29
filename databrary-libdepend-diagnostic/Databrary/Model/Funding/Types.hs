{-# LANGUAGE OverloadedStrings, RecordWildCards, TemplateHaskell, TypeFamilies #-}
module Databrary.Model.Funding.Types
  ( Funder(..)
  , Funding(..)
  ) where

import Data.Int (Int64)
import qualified Data.Text as T

import Databrary.Has (makeHasRec)
import Databrary.Model.Kind
import Databrary.Model.Id.Types

type instance IdType Funder = Int64

data Funder = Funder
  { funderId :: Id Funder
  , funderName :: T.Text
  }

makeHasRec ''Funder ['funderId]

instance Kinded Funder where
  kindOf _ = "funder"

data Funding = Funding
  { fundingFunder :: Funder
  , fundingAwards :: [T.Text]
  }

makeHasRec ''Funding ['fundingFunder]
