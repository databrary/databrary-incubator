{-# LANGUAGE TemplateHaskell #-}
module Databrary.Model.Activity.SQL
  ( selectActivityParty
  , selectActivityAccount
  , selectActivityAuthorize
  , selectActivityVolume
  , selectActivityAccess
  , selectActivityContainer
  , selectActivityRelease
  , selectActivityAsset
  , selectActivityAssetSlot
  , selectActivityExcerpt
  , activityQual
  ) where

import Data.List (stripPrefix)
import qualified Language.Haskell.TH as TH

import Databrary.Model.SQL.Select
import Databrary.Model.Audit.Types
import Databrary.Model.Audit.SQL
import Databrary.Model.Party.SQL
import Databrary.Model.Party.Types
import Databrary.Model.Authorize.SQL
import Databrary.Model.Volume.SQL
import Databrary.Model.VolumeAccess.SQL
import Databrary.Model.Container.SQL
import Databrary.Model.Container.Types (ContainerRow(..))
import Databrary.Model.Slot.Types
import Databrary.Model.Release.SQL
import Databrary.Model.Asset.SQL
import Databrary.Model.Activity.Types

delim :: String -> Bool
delim "" = True
delim (' ':_) = True
delim (',':_) = True
delim _ = False

makeActivity :: Audit -> ActivityTarget -> Activity
makeActivity a x = Activity a x Nothing Nothing Nothing

targetActivitySelector :: String -> Selector -> Selector  -- TODO: remove
targetActivitySelector t Selector{ selectOutput = o, selectSource = ts, selectJoined = (',':tj) }
  | Just s <- stripPrefix t ts, delim s, ts == tj =
    selector ("audit." ++ ts) $ OutputJoin False 'makeActivity [selectOutput (selectAudit t), o]
targetActivitySelector t Selector{ selectSource = ts } = error $ "targetActivitySelector " ++ t ++ ": " ++ ts

selectActivityParty :: Selector
selectActivityParty = targetActivitySelector "party" $
  selectMap (TH.ConE 'ActivityParty `TH.AppE`) (selectColumns 'PartyRow "party" ["id", "name", "prename", "orcid", "affiliation", "url"])

selectActivityAccount :: Selector
selectActivityAccount = targetActivitySelector "account" $
  selectColumns 'ActivityAccount "account" ["id", "email", "password"]

selectActivityAuthorize :: TH.Name -> TH.Name -> Selector
selectActivityAuthorize p ident = targetActivitySelector "authorize" $
  selectMap (TH.ConE 'ActivityAuthorize `TH.AppE`) (selectAuthorizeChild p ident)

selectActivityVolume :: Selector
selectActivityVolume = targetActivitySelector "volume" $
  selectMap (TH.ConE 'ActivityVolume `TH.AppE`) selectVolumeRow

selectActivityAccess :: TH.Name -> TH.Name -> Selector
selectActivityAccess vol ident = targetActivitySelector "volume_access" $
  selectMap (TH.ConE 'ActivityAccess `TH.AppE`) (selectVolumeAccess vol ident)

selectActivityContainer :: Selector
selectActivityContainer = targetActivitySelector "container" $
  selectMap 
    (TH.ConE 'ActivityContainer `TH.AppE`) 
    (selectColumns 'ContainerRow "container" ["id", "top", "name", "date"])

selectActivityRelease :: Selector
selectActivityRelease = targetActivitySelector "slot_release" $
  addSelects 'ActivityRelease (selectColumns 'SlotId "slot_release" ["container", "segment"]) [selectOutput (selectColumn "slot_release" "release")]

selectActivityAsset :: Selector
selectActivityAsset = targetActivitySelector "asset" $
  selectMap (TH.ConE 'ActivityAsset `TH.AppE`) selectAssetRow

selectActivityAssetSlot :: Selector
selectActivityAssetSlot = targetActivitySelector "slot_asset" $
  addSelects 'ActivityAssetSlot
    (selectColumn "slot_asset" "asset")
    [selectOutput $ selectColumns 'SlotId "slot_asset" ["container", "segment"]]

selectActivityExcerpt :: Selector
selectActivityExcerpt = targetActivitySelector "excerpt" $
  selectColumns 'ActivityExcerpt "excerpt" ["asset", "segment", "release"]

activityQual :: String
activityQual = "audit_action >= 'add' ORDER BY audit_time"
