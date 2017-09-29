{-# LANGUAGE OverloadedStrings #-}
module Databrary.View.VolumeAccess
  ( volumeAccessTitle
  , volumeAccessPresetTitle
  , htmlVolumeAccessForm
  ) where

import qualified Data.ByteString.Char8 as BSC
import Data.Monoid ((<>))
import qualified Data.Text as T

import qualified Databrary.Store.Config as C
import Databrary.Service.Messages
import Databrary.Action
import Databrary.Model.Party
import Databrary.Model.Permission
import Databrary.Model.Volume
import Databrary.Model.VolumeAccess
import Databrary.Controller.Paths
import Databrary.View.Form

import {-# SOURCE #-} Databrary.Controller.VolumeAccess

volumeAccessTitle :: Permission -> Messages -> T.Text
volumeAccessTitle perm = getMessage $ C.Path ["access", "edit", BSC.pack (show perm), "title"]

volumeAccessPresetTitle :: Bool -> Messages -> T.Text
volumeAccessPresetTitle shared = getMessage $ C.Path ["access", "preset", "title" <> BSC.pack (show (fromEnum shared))]

htmlVolumeAccessForm :: VolumeAccess -> RequestContext -> FormHtml f
htmlVolumeAccessForm a@VolumeAccess{ volumeAccessVolume = vol, volumeAccessParty = p } = htmlForm
  ("Access to " <> volumeName (volumeRow vol) <> " for " <> partyName (partyRow p))
  postVolumeAccess (HTML, (volumeId $ volumeRow vol, VolumeAccessTarget $ partyId $ partyRow p))
  (do
    field "individual" $ inputEnum True $ Just $ volumeAccessIndividual a
    field "children" $ inputEnum True $ Just $ volumeAccessChildren a
    field "delete" $ inputCheckbox False)
  (const mempty)
