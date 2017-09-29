{-# LANGUAGE TemplateHaskell #-}
module Databrary.Model.AssetRevision.SQL
  ( selectAssetRevision
  ) where

import qualified Language.Haskell.TH as TH

import Databrary.Model.SQL.Select
import Databrary.Model.Asset.Types
import Databrary.Model.Asset.SQL
import Databrary.Model.AssetRevision.Types

makeAssetRevision :: Asset -> Asset -> AssetRevision
makeAssetRevision o a = AssetRevision a o

selectAssetRevision :: String -- ^ table
  -> TH.Name -- ^ @'Identity'@
  -> Selector -- ^ @'Asset' -> 'AssetRevision'@
selectAssetRevision table ident = selectJoin '($)
  [ selectColumns 'makeAssetRevision table []
  , joinOn (table ++ ".orig = asset.id")
    $ selectAsset ident
  ]
