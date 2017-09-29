{-# LANGUAGE TemplateHaskell, DataKinds #-}
module Databrary.Model.Category.Boot
  ( loadCategories
  ) where

import Database.PostgreSQL.Typed.Array ()
import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Syntax as TH

import Databrary.Service.DB
import Databrary.Model.SQL (selectQuery)
import Databrary.Model.SQL.Select
import Databrary.Model.Category.Types

loadCategories :: TH.ExpQ -- [Category]
loadCategories = do
  l <- 
    runTDB $ 
      dbQuery $(selectQuery (selectColumns 'Category "category" ["id", "name", "description"]) "ORDER BY id")
  TH.lift l
