{-# LANGUAGE OverloadedStrings #-}
module Databrary.Routes.JS
  ( jsRoutes
  ) where

import qualified Data.ByteString.Builder as B

import Databrary.Model.Id.Types
import Databrary.Model.Segment
import Databrary.Model.Slot.Types
import Databrary.Model.Tag.Types
import Databrary.Action
import Databrary.Controller.Paths
import Databrary.Controller.Root
import Databrary.Controller.Login
import Databrary.Controller.Register
import Databrary.Controller.Token
import Databrary.Controller.Party
import Databrary.Controller.Authorize
import Databrary.Controller.Volume
import Databrary.Controller.VolumeAccess
import Databrary.Controller.Funding
import Databrary.Controller.Container
import Databrary.Controller.Slot
import Databrary.Controller.Record
import Databrary.Controller.Metric
import Databrary.Controller.Citation
import Databrary.Controller.Upload
import Databrary.Controller.Format
import Databrary.Controller.Asset
import Databrary.Controller.AssetSegment
import Databrary.Controller.Excerpt
import Databrary.Controller.Zip
import Databrary.Controller.Tag
import Databrary.Controller.Comment
import Databrary.Controller.CSV
import Databrary.Controller.VolumeState
import Databrary.Controller.Search
import Databrary.Controller.Activity
import Databrary.Controller.Notification
import Databrary.Web.Routes

jsRoutes :: [B.Builder] -- should be mconcat, but BSB bug causes hangs
jsRoutes =
  [ jsRoute "viewRoot" viewRoot (HTML)
  , jsRoute "viewLogin" viewLogin ()
  , jsRoute "viewRegister" viewRegister ()
  , jsRoute "viewPasswordReset" viewPasswordReset ()
  , jsRoute "viewLoginToken" viewLoginToken (HTML, token)

  , jsRoute "viewProfile" viewParty (HTML, TargetProfile)
  , jsRoute "viewProfileEdit" viewPartyEdit (TargetProfile)
  , jsRoute "viewParty" viewParty (HTML, TargetParty party)
  , jsRoute "viewPartyEdit" viewPartyEdit (TargetParty party)
  , jsRoute "viewPartySearch" queryParties (HTML)
  , jsRoute "partyAvatar" viewAvatar (party)
  , jsRoute "viewPartyActivity" viewPartyActivity (HTML, TargetParty party)

  , jsRoute "viewVolume" viewVolume (HTML, volume)
  , jsRoute "viewVolumeCreate" viewVolumeCreate ()
  , jsRoute "viewVolumeEdit" viewVolumeEdit (volume)
  , jsRoute "viewVolumeSearch" queryVolumes (HTML)
  , jsRoute "thumbVolume" thumbVolume (volume)
  , jsRoute "csvVolume" csvVolume (volume)
  , jsRoute "viewVolumeActivity" viewVolumeActivity (HTML, volume)

  , jsRoute "viewSlot" viewSlot (HTML, (Just volume, slot))
  , jsRoute "viewSlotEdit" viewContainerEdit (Just volume, container)
  , jsRoute "viewSlotActivity" viewContainerActivity (HTML, (Just volume, container))
  , jsRoute "thumbSlot" thumbSlot (Just volume, slot)

  , jsRoute "viewRecord" viewRecord (HTML, record)

  , jsRoute "viewFormats" viewFormats ()
  , jsRoute "viewAssetSegment" viewAssetSegment (HTML, Just volume, slot, asset)
  , jsRoute "downloadAssetSegment" downloadAssetSegment (slot, asset)
  , jsRoute "thumbAssetSegment" thumbAssetSegment (slot, asset)
  , jsRoute "downloadAsset" downloadAsset (asset, fullSegment)
  , jsRoute "thumbAsset" thumbAsset (asset, fullSegment)

  , jsRoute "viewSearch" postSearch (HTML)

  , jsRoute "zipSlot" zipContainer (Just volume, container)
  , jsRoute "zipVolume" zipVolume (volume)
  , jsRoute "viewVolumeDescription" viewVolumeDescription (volume)

  , jsRoute "get" viewRoot (JSON)
  , jsRoute "getUser" viewUser ()
  , jsRoute "postUser" postUser (JSON)
  , jsRoute "postLogin" postLogin (JSON)
  , jsRoute "postLogout" postLogout (JSON)
  , jsRoute "postRegister" postRegister (JSON)
  , jsRoute "postPasswordReset" postPasswordReset (JSON)
  , jsRoute "getLoginToken" viewLoginToken (JSON, token)
  , jsRoute "postPasswordToken" postPasswordToken (JSON, token)

  , jsRoute "getParty" viewParty (JSON, TargetParty party)
  , jsRoute "getProfile" viewParty (JSON, TargetProfile)
  , jsRoute "postParty" postParty (JSON, TargetParty party)
  , jsRoute "getParties" queryParties (JSON)
  , jsRoute "getPartyActivity" viewPartyActivity (JSON, TargetParty party)

  , jsRoute "postAuthorizeApply" postAuthorize (JSON, TargetParty party, AuthorizeTarget True party)
  , jsRoute "postAuthorize" postAuthorize (JSON, TargetParty party, AuthorizeTarget False party)
  , jsRoute "deleteAuthorize" deleteAuthorize (JSON, TargetParty party, AuthorizeTarget False party)
  , jsRoute "deleteAuthorizeParent" deleteAuthorize (JSON, TargetParty party, AuthorizeTarget True party)
  , jsRoute "postAuthorizeNotFound" postAuthorizeNotFound (TargetParty party)

  , jsRoute "getVolume" viewVolume (JSON, volume)
  , jsRoute "postVolume" postVolume (JSON, volume)
  , jsRoute "createVolume" createVolume (JSON)
  , jsRoute "getVolumes" queryVolumes (JSON)
  , jsRoute "postVolumeAccess" postVolumeAccess (JSON, (volume, VolumeAccessTarget party))
  , jsRoute "postVolumeFunding" postVolumeFunding (volume, funder)
  , jsRoute "postVolumeLinks" postVolumeLinks (JSON, volume)
  , jsRoute "deleteVolumeFunder" deleteVolumeFunder (volume, funder)
  , jsRoute "postVolumeAssist" postVolumeAssist (volume)
  , jsRoute "getVolumeActivity" viewVolumeActivity (JSON, volume)

  , jsRoute "postSearch" postSearch (JSON)
  , jsRoute "getFunders" queryFunder ()
  , jsRoute "getCitation" getCitation ()

  , jsRoute "getSlot" viewSlot (JSON, (Nothing, slot))
  , jsRoute "postContainer" postContainer (JSON, container)
  , jsRoute "deleteContainer" deleteContainer (JSON, container)
  , jsRoute "createContainer" createContainer (JSON, volume)
  , jsRoute "getContainerActivity" viewContainerActivity (JSON, (Nothing, container))

  , jsRoute "getRecord" viewRecord (JSON, record)
  , jsRoute "createRecord" createRecord (JSON, volume)
  , jsRoute "deleteRecord" deleteRecord (JSON, record)
  , jsRoute "postRecordMeasure" postRecordMeasure (JSON, record, metric)
  , jsRoute "postRecordSlot" postRecordSlot (JSON, slot, record)
  , jsRoute "deleteRecordAllSlot" deleteRecordAllSlot (JSON, record)

  , jsRoute "addVolumeMetric" postVolumeMetric (volume, Right metric)
  , jsRoute "addVolumeCategory" postVolumeMetric (volume, Left category)
  , jsRoute "deleteVolumeMetric" deleteVolumeMetric (volume, Right metric)
  , jsRoute "deleteVolumeCategory" deleteVolumeMetric (volume, Left category)
  , jsRoute "postVolumeState" postVolumeState (volume, "")
  , jsRoute "deleteVolumeState" deleteVolumeState (volume, "")

  , jsRoute "getAsset" viewAsset (JSON, asset)
  , jsRoute "getAssetSegment" viewAssetSegment (JSON, Just volume, slot, asset)
  , jsRoute "postAsset" postAsset (JSON, asset)
  , jsRoute "createAsset" createAsset (JSON, volume)
  , jsRoute "deleteAsset" deleteAsset (JSON, asset)
  , jsRoute "postExcerpt" postExcerpt (slot, asset)
  , jsRoute "deleteExcerpt" deleteExcerpt (slot, asset)
  , jsRoute "uploadStart" uploadStart (volume)
  , jsRoute "uploadChunk" uploadChunk ()

  , jsRoute "postComment" postComment (JSON, slot)
  , jsRoute "getTags" queryTags (Just tag)
  , jsRoute "postTag" postTag (JSON, slot, TagId False tag)
  , jsRoute "postKeyword" postTag (JSON, slot, TagId True tag)
  , jsRoute "deleteTag" deleteTag (JSON, slot, TagId False tag)
  , jsRoute "deleteKeyword" deleteTag (JSON, slot, TagId True tag)
  , jsRoute "getTopTags" queryTags Nothing
  , jsRoute "getSiteActivity" viewSiteActivity (JSON)

  , jsRoute "getNotifications" viewNotifications ()
  , jsRoute "deleteNotification" deleteNotification (Id 0)
  , jsRoute "deleteNotifications" deleteNotifications ()
  , jsRoute "getNotify" viewNotify ()
  , jsRoute "postNotify" postNotify ()
  ] where
  token = Id ""
  party = Id 0
  volume = Id 0
  slot = Id (SlotId (Id 0) emptySegment)
  container = containerSlotId (Id 0)
  asset = Id 0
  record = Id 0
  category = Id 0
  metric = Id 0
  funder = Id 0
  tag = TagName ""
