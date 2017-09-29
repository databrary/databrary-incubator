{-# LANGUAGE TemplateHaskell #-}
module Databrary.Model.Permission.SQL
  ( accessRow
  , accessSets
  ) where

import Databrary.Model.SQL.Select
import Databrary.Model.Permission.Types

accessRow :: String -- ^ Table name
  -> Selector -- ^ 'Access'
accessRow table = selectColumns 'Access table ["site", "member"]

accessSets :: String -- ^ @'Access'@
  -> [(String, String)]
accessSets a =
  [ ("site", "${accessSite " ++ a ++ "}")
  , ("member", "${accessMember " ++ a ++ "}")
  ]
