{-# LANGUAGE OverloadedStrings #-}
module Databrary.Controller.Permission
  ( checkPermission
  , checkDataPermission
  , authAccount
  , checkMemberADMIN
  , checkVerfHeader
  , guardVerfHeader
  ) where

import Control.Monad (void, unless, liftM2)

import Databrary.Has (Has, view, peek, peeks)
import Databrary.Model.Permission
import Databrary.Model.Release
import Databrary.Model.Party
import Databrary.Model.Identity
import Databrary.HTTP.Request
import Databrary.Action

checkPermission :: Has Permission a => Permission -> a -> ActionM a
checkPermission p o = do
  unless (view o >= p) $ result =<< peeks forbiddenResponse
  return o

checkDataPermission :: (Has Release a, Has Permission a) => a -> ActionM a
checkDataPermission o = do
  unless (dataPermission o > PermissionNONE) $ result =<< peeks forbiddenResponse
  return o

authAccount :: ActionM Account
authAccount = do
  ident <- peek
  case ident of
    PreIdentified -> fail "authAccount: PreIdentified"
    NotIdentified -> result =<< peeks forbiddenResponse
    Identified s -> return $ view s
    ReIdentified u -> return $ view u

checkMemberADMIN :: ActionM ()
checkMemberADMIN = do
  admin <- peeks accessMember'
  void $ checkPermission PermissionADMIN admin

checkVerfHeader :: ActionM Bool
checkVerfHeader = do
  header <- peeks $ lookupRequestHeader "x-csverf"
  peeks $ or . liftM2 (==) header . identityVerf

guardVerfHeader :: ActionM ()
guardVerfHeader = do
  c <- checkVerfHeader
  unless c $ result =<< peeks forbiddenResponse
