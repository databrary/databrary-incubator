{-# LANGUAGE TemplateHaskell, TypeFamilies, OverloadedStrings #-}
module Databrary.Model.Category.Types
  ( Category(..)
  ) where

import Data.Function (on)
import Data.Int (Int16)
import Data.Ord (comparing)
import qualified Data.Text as T
import Instances.TH.Lift ()
import Language.Haskell.TH.Lift (deriveLift)

import Databrary.Has (makeHasRec)
import Databrary.Model.Kind
import Databrary.Model.Id.Types

type instance IdType Category = Int16

data Category = Category
  { categoryId :: !(Id Category)
  , categoryName :: !T.Text
  , categoryDescription :: !(Maybe T.Text)
  }

instance Kinded Category where
  kindOf _ = "category"

instance Eq Category where
  (==) = on (==) categoryId
  (/=) = on (/=) categoryId

instance Ord Category where
  compare = comparing categoryId

makeHasRec ''Category ['categoryId]
deriveLift ''Category
