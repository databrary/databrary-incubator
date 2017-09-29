{-# LANGUAGE OverloadedStrings, TemplateHaskell, RecordWildCards #-}
module Databrary.Model.Category
  ( module Databrary.Model.Category.Types
  , allCategories
  , getCategory
  , getCategory'
  , categoryJSON
  ) where

import qualified Data.IntMap.Strict as IntMap
import Data.Monoid ((<>))

import qualified Databrary.JSON as JSON
import Databrary.Model.Id
import Databrary.Model.Category.Types
import Databrary.Model.Category.Boot

allCategories :: [Category]
allCategories = $(loadCategories)

categoriesById :: IntMap.IntMap Category
categoriesById = IntMap.fromAscList $ map (\a -> (fromIntegral $ unId $ categoryId a, a)) allCategories

getCategory :: Id Category -> Maybe Category
getCategory (Id i) = IntMap.lookup (fromIntegral i) categoriesById

getCategory' :: Id Category -> Category
getCategory' (Id i) = categoriesById IntMap.! fromIntegral i

categoryJSON :: JSON.ToObject o => Category -> JSON.Record (Id Category) o
categoryJSON Category{..} = JSON.Record categoryId $
     "name" JSON..= categoryName
  <> "description" JSON..=? categoryDescription
