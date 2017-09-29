{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Databrary.Model.Party.SQL
  ( -- selectPartyRow
    selectParty
  , selectPartyAuthorization
  , selectAuthParty
  , selectAccount
  , selectUserAccount
  , selectSiteAuth
  , updateParty
  , updateAccount
  , insertParty
  , insertAccount
  , deleteParty
  , deleteAccount
  ) where

import qualified Data.ByteString as BS
import Data.Foldable (fold)
import Data.Monoid ((<>))
import qualified Language.Haskell.TH as TH

import Databrary.Has (Has, view)
import Databrary.Model.SQL.Select
import Databrary.Model.Audit.SQL
import Databrary.Model.Permission.Types
import Databrary.Model.Permission.SQL
import Databrary.Model.Id.Types
import Databrary.Model.Identity.Types
import Databrary.Model.Party.Types

makeParty :: PartyRow -> Maybe (Party -> Account) -> Permission -> Maybe Access -> Party
makeParty pr ac perm a = p where
  p = Party pr (fmap ($ p) ac) perm a

selectPermissionParty :: Selector -- ^ @'Permission' -> Maybe 'Access' -> 'Party'@
selectPermissionParty = selectJoin 'makeParty
  [ selectColumns 'PartyRow "party" ["id", "name", "prename", "orcid", "affiliation", "url"]
  , maybeJoinUsing ["id"] (selectColumns 'Account "account" ["email"])
  ]

permissionParty :: Has (Id Party) a => (Permission -> Maybe Access -> a) -> Maybe Access -> Identity -> a
permissionParty pf a' ident = p where
  p = pf
    (maybe id (max . accessPermission') a $ max PermissionPUBLIC $ min PermissionREAD $ accessSite ident)
    a
  a | foldIdentity False (((view p :: Id Party) ==) . view) ident = Just maxBound
    | identityAdmin ident = Just $ maybe id (<>) a' $ view ident
    | otherwise = a'

selectParty :: TH.Name -- ^ 'Identity'
  -> Selector -- ^ @'Party'@
selectParty ident = selectMap ((`TH.AppE` TH.VarE ident) . (`TH.AppE` (TH.ConE 'Nothing)) . (TH.VarE 'permissionParty `TH.AppE`)) $
  selectPermissionParty

makePartyAuthorization :: Party -> Maybe Access -> (Party, Maybe Permission)
makePartyAuthorization p a = (p, accessSite <$> a)

selectPartyAuthorization :: TH.Name -- ^ 'Identity'
  -> Selector -- ^ @('Party', Maybe 'Permission')@
selectPartyAuthorization ident = selectJoin 'makePartyAuthorization
  [ selectParty ident
  , maybeJoinOn "party.id = authorize_view.child AND authorize_view.parent = 0"
    $ accessRow "authorize_view"
  ]

selectAuthParty :: TH.Name -- ^ 'Identity`
  -> Selector -- ^ @'Party'@
selectAuthParty ident = selectMap (`TH.AppE` TH.VarE ident) $ selectJoin 'permissionParty
  [ selectPermissionParty
  , maybeJoinOn ("party.id = authorize_valid.parent AND authorize_valid.child = ${view " ++ nameRef ident ++ " :: Id Party}")
    $ accessRow "authorize_valid" -- optimization, should be authorize_view if we used site
  ]

makeAccount :: PartyRow -> (Party -> Account) -> Permission -> Maybe Access -> Account
makeAccount pr ac perm ma = a where
  a = ac $ Party pr (Just a) perm ma

selectPermissionAccount :: Selector -- ^ @'Permission' -> Maybe 'Access' -> 'Account'@
selectPermissionAccount = selectJoin 'makeAccount
  [ selectColumns 'PartyRow "party" ["id", "name", "prename", "orcid", "affiliation", "url"]
  , joinUsing ["id"] (selectColumns 'Account "account" ["email"])
  ]

selectAccount :: TH.Name -- ^ 'Identity'
  -> Selector -- ^ @'Account'@
selectAccount ident = selectMap ((`TH.AppE` TH.VarE ident) . (`TH.AppE` (TH.ConE 'Nothing)) . (TH.VarE 'permissionParty `TH.AppE`)) $
  selectPermissionAccount

makeUserAccount :: (Permission -> Maybe Access -> Account) -> Account
makeUserAccount a = a maxBound (Just maxBound)

selectUserAccount :: Selector -- @'Account'
selectUserAccount = selectMap (TH.VarE 'makeUserAccount `TH.AppE`) selectPermissionAccount

makeSiteAuth :: Account -> Maybe BS.ByteString -> Maybe Access -> SiteAuth
makeSiteAuth p w a = SiteAuth p w (fold a)

selectSiteAuth :: Selector -- @'SiteAuth'@
selectSiteAuth = selectJoin 'makeSiteAuth
  [ selectUserAccount
  , Selector (SelectColumn "account" "password") "" ""
  , maybeJoinOn "account.id = authorize_view.child AND authorize_view.parent = 0"
    $ accessRow "authorize_view"
  ]

partyKeys :: String -- ^ @'Party'@
  -> [(String, String)]
partyKeys p =
  [ ("id", "${partyId $ partyRow " ++ p ++ "}") ]

accountKeys :: String -- ^ @'Account'@
  -> [(String, String)]
accountKeys a = partyKeys $ "(accountParty " ++ a ++ ")"

partySets :: String -- ^ @'Party'@
  -> [(String, String)]
partySets p =
  [ ("name",        "${partySortName $ partyRow "    ++ p ++ "}")
  , ("prename",     "${partyPreName $ partyRow "     ++ p ++ "}")
  , ("affiliation", "${partyAffiliation $ partyRow " ++ p ++ "}")
  , ("url",         "${partyURL $ partyRow "         ++ p ++ "}")
  ]

accountSets :: String -- ^ @'Account'@
  -> [(String, String)]
accountSets a =
  [ ("email", "${accountEmail " ++ a ++ "}")
  ]

updateParty :: TH.Name -- ^ @'AuditIdentity'
  -> TH.Name -- ^ @'Party'@
  -> TH.ExpQ -- ()
updateParty ident p = auditUpdate ident "party"
  (partySets ps)
  (whereEq $ partyKeys ps)
  Nothing
  where ps = nameRef p

updateAccount :: TH.Name -- ^ @'AuditIdentity'
  -> TH.Name -- ^ @'Account'@
  -> TH.ExpQ -- ()
updateAccount ident a = auditUpdate ident "account"
  (accountSets as ++ [("password", "${accountPasswd " ++ us ++ "}")])
  (whereEq $ accountKeys as)
  Nothing
  where
  as = "(siteAccount " ++ us ++ ")"
  us = nameRef a

insertParty :: TH.Name -- ^ @'AuditIdentity'
  -> TH.Name -- ^ @'Party'@
  -> TH.ExpQ -- ^ @'PartyRow'@
insertParty ident p = auditInsert ident "party"
  (partySets ps)
  (Just $ selectOutput (selectColumns 'PartyRow "party" ["id", "name", "prename", "orcid", "affiliation", "url"]))
  where ps = nameRef p

insertAccount :: TH.Name -- ^ @'AuditIdentity'
  -> TH.Name -- ^ @'Account'@
  -> TH.ExpQ
insertAccount ident a = auditInsert ident "account"
  (accountKeys as ++ accountSets as)
  Nothing
  where as = nameRef a

deleteParty :: TH.Name -- ^ @'AuditIdentity'
  -> TH.Name -- ^ @'Party'@
  -> TH.ExpQ -- ^ @()@
deleteParty ident p = auditDelete ident "party"
  (whereEq $ partyKeys (nameRef p))
  Nothing

deleteAccount :: TH.Name -- ^ @'AuditIdentity'
  -> TH.Name -- ^ @'Party'@
  -> TH.ExpQ -- ^ @()@
deleteAccount ident p = auditDelete ident "account"
  (whereEq $ partyKeys (nameRef p))
  Nothing
