{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes, DeriveDataTypeable, DataKinds #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Databrary.Model.Permission.Types
  ( Permission(..)
  , Access(..), accessPermission'
  , accessSite, accessMember, accessPermission
  ) where

import Language.Haskell.TH.Lift (deriveLiftMany)

import Databrary.Has (Has(..))
import Databrary.Model.Enum

makeDBEnum "permission" "Permission"

instance Monoid Permission where
  mempty = PermissionNONE
  mappend = max

data Access = Access
  { accessSite' :: !Permission
  , accessMember' :: !Permission
  } deriving (Eq)

accessPermission' :: Access -> Permission
accessPermission' (Access s m) = min s m

accessSite, accessMember, accessPermission :: Has Access a => a -> Permission
accessSite = accessSite' . view
accessMember = accessMember' . view
accessPermission = accessPermission' . view

instance Bounded Access where
  minBound = Access minBound minBound
  maxBound = Access maxBound maxBound

instance Monoid Access where
  mempty = Access mempty mempty
  mappend (Access s1 m1) (Access s2 m2) = Access (mappend s1 s2) (mappend m1 m2)

deriveLiftMany [''Permission, ''Access]
