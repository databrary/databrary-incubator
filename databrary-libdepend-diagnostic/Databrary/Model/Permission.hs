{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes, RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Databrary.Model.Permission
  ( module Databrary.Model.Permission.Types
  , permissionVIEW
  , permissionPRIVATE
  , readPermission
  , readRelease
  , dataPermission
  , accessJSON
  ) where

import Data.Monoid ((<>))

import Databrary.Has (Has, view)
import qualified Databrary.JSON as JSON
import Databrary.Model.Release.Types
import Databrary.Model.Permission.Types

-- |Level at which things become visible.
permissionVIEW :: Permission
permissionVIEW = PermissionPUBLIC

-- |Alias for READ. Grants full access to private data, bypassing consent permissions.
permissionPRIVATE :: Permission
permissionPRIVATE = PermissionREAD

-- |The necessary permission level to read a data object with the given release.
-- Equivalent to the SQL function read_permission.
readPermission :: Release -> Permission
readPermission ReleasePUBLIC   = PermissionPUBLIC
readPermission ReleaseSHARED   = PermissionSHARED
readPermission ReleaseEXCERPTS = PermissionSHARED
readPermission ReleasePRIVATE  = permissionPRIVATE

-- |The most restrictive data release level that the current user may access under the given permission.
-- Equivalent to the SQL function read_release.  Inverse of 'readPermission' module meaning of @Nothing@.
readRelease :: Permission -> Maybe Release
readRelease PermissionNONE   = Nothing
readRelease PermissionPUBLIC = Just ReleasePUBLIC
readRelease PermissionSHARED = Just ReleaseSHARED
readRelease _                = Just ReleasePRIVATE

-- |The effective permission for data objects with the given attributes, effectively collapsing ineffective permissions NONE.
releasePermission :: Release -> Permission -> Permission
releasePermission r p
  | p >= readPermission r = p
  | otherwise = PermissionNONE

dataPermission :: (Has Release a, Has Permission a) => a -> Permission
dataPermission a = releasePermission (view a) (view a)

accessJSON :: JSON.ToObject o => Access -> o
accessJSON Access{..} =
     "site" JSON..= accessSite'
  <> "member" JSON..= accessMember'
