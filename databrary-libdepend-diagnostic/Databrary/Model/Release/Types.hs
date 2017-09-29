{-# LANGUAGE TemplateHaskell, OverloadedStrings, DeriveDataTypeable, DataKinds #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Databrary.Model.Release.Types
  ( Release(..)
  ) where

import Data.Foldable (fold)
import Language.Haskell.TH.Lift (deriveLift)

import Databrary.Has (Has(..))
import Databrary.Model.Enum

makeDBEnum "release" "Release"

instance Monoid Release where
  mempty = ReleasePRIVATE
  mappend = max

instance Has Release (Maybe Release) where
  view = fold

deriveLift ''Release
