{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Databrary.View.Notification
  ( mailNotifications
  , htmlNotification
  ) where

import Control.Arrow (second)
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Char8 as BSC
import Data.Function (on)
import Data.Maybe (fromMaybe, isJust)
import Data.Monoid ((<>))
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as HA

import Databrary.Ops
import Databrary.Model.Permission
import Databrary.Model.Id.Types
import Databrary.Model.Party
import Databrary.Model.Volume.Types
import Databrary.Model.Segment
import Databrary.Model.Slot.Types
import Databrary.Model.Tag.Types
import Databrary.Model.Notification
import Databrary.Service.Messages
import Databrary.HTTP.Route
import Databrary.Action.Route
import Databrary.Controller.Paths
import {-# SOURCE #-} Databrary.Controller.Party
import {-# SOURCE #-} Databrary.Controller.Volume
import {-# SOURCE #-} Databrary.Controller.Slot
import {-# SOURCE #-} Databrary.Controller.AssetSegment
import Databrary.View.Authorize (authorizeSiteTitle)
import Databrary.View.Party (htmlPartyViewLink)
import Databrary.View.Volume (htmlVolumeViewLink)
import Databrary.View.VolumeAccess (volumeAccessTitle, volumeAccessPresetTitle)
import Databrary.View.Container (releaseTitle)
import Databrary.View.Html

mailLink :: Route r a -> a -> [(BSC.ByteString, BSC.ByteString)] -> TL.Text
mailLink u a q = TLE.decodeLatin1 $ BSB.toLazyByteString $ "https://databrary.org" <> actionURL Nothing u a (map (second Just) q :: Query)

partyEditLink :: (ActionRoute PartyTarget -> PartyTarget -> t) -> PartyRow -> PartyRow -> t
partyEditLink link target p = link viewPartyEdit (if on (==) partyId p target then TargetProfile else TargetParty (partyId p))

mailNotification :: Messages -> Notification -> TL.Text
mailNotification msg Notification{..} = case notificationNotice of
  NoticeAccountChange ->
    party'S <> " email or password has been changed. If you made this change, you may ignore this email. To review or update your account information, go to: "
    <> partyEdit (fromMaybe target notificationParty) [("page", "account")]
    <> "\nIf you did not make this change, please contact us immediately."
  NoticeAuthorizeRequest ->
    agent <> " requested authorization from " <> party <> ". To review the status of this request, go to: "
    <> partyEdit target [("page", "apply"), partyq]
  NoticeAuthorizeGranted
    | Just p <- notificationPermission ->
      "You have been authorized under " <> party <> ", "
      <> if p == PermissionNONE then "for group/lab-only access."
         else "as a Databrary " <> TL.fromStrict (authorizeSiteTitle p msg) <> ". \
      \Your authorization allows you to access all the shared data in Databrary. \
      \Our primary goal is to inspire you to reuse shared videos on Databrary to ask new questions outside the scope of the original study. \
      \You will also find illustrative video excerpts that you can use for teaching and to learn about other researchers' methods and procedures.\
      \\n\n\
      \Databrary's unique \"active curation\" functionality allows you to upload your videos as you collect them so that your data are backed up and preserved in our free, secure library, your videos are immediately available to you and your collaborators offsite, and your data are organized and ready for sharing. \
      \Your data will remain private and accessible only to your lab members and collaborators until you are ready to share with the Databrary community. \
      \When you are ready, sharing is as easy as clicking a button!\
      \\n\n\
      \You can use our template Databrary release form to obtain permission for sharing the data you collect from your participants, which can be found here: http://databrary.org/access/policies/release-template.html\n\
      \The release form can be added to new or existing IRB protocols. \
      \It is completely adaptable and can be customized to suit your needs. \
      \We also offer additional information and helpful tips about managing and sharing your video data in our User Guide: http://databrary.org/access/guide\n\
      \As soon as your protocol is amended to allow you to share data, you can start uploading your data from each new session. \
      \Don't wait until your study is complete to upload your videos. \
      \It's much easier to upload data after each data collection while your study is in progress!\
      \\n\n\
      \We are dedicated to providing assistance to the Databrary community. \
      \Please contact us at support@databrary.org with questions or for help getting started.\
      \\n"
    | otherwise ->
      "Your authorization under " <> party <> " has been revoked. To review and apply for authorizations, go to: "
      <> partyEdit target [("page", "apply")]
  NoticeAuthorizeExpiring ->
    "Your authorization under " <> party <> " will expire within a week. Please contact them and request that they renew your authorization."
  NoticeAuthorizeExpired ->
    "Your authorization under " <> party <> " has expired. Please contact them and request that they renew your authorization."
  NoticeAuthorizeChildRequest ->
    agent <> " has requested to be authorized through " <> party <> ". To approve or reject this authorization request, go to: "
    <> partyEdit (fromMaybe target notificationParty) [("page", "grant"), personq $ Just notificationAgent]
  NoticeAuthorizeChildGranted ->
    agent <> " " <> granted <> " authorization to " <> party <> ". To review this authorization, go to: "
    <> partyEdit target [("page", "grant"), partyq]
  NoticeAuthorizeChildExpiring ->
    party'S <> " authorization will expire within a week. If you would like to renew their authorization, go to: "
    <> partyEdit target [("page", "grant"), partyq]
  NoticeAuthorizeChildExpired ->
    party'S <> " authorization has expired. If you would like to renew their authorization, go to: "
    <> partyEdit target [("page", "grant"), partyq]
  NoticeVolumeAssist ->
    agent <> " requested assistance with your volume, " <> volume <> ". To review this request, go to: "
    <> volumeEdit [("page", "assist")]
  NoticeVolumeCreated ->
    agent <> " created a volume, " <> volume <> ", on " <> party's <> " behalf. To review this volume, go to: "
    <> mailLink viewVolume (HTML, maybe noId volumeId notificationVolume) []
  NoticeVolumeSharing ->
    agent <> " changed your volume, " <> volume <> ", to " <> TL.fromStrict (volumeAccessPresetTitle (PermissionNONE < perm) msg) <> ". To review this change, go to: "
    <> volumeEdit [("page", "access")]
  NoticeVolumeAccessOther ->
    agent <> " set " <> party's <> " access to " <> TL.fromStrict (volumeAccessTitle perm msg) <> " on your volume, " <> volume <> ". To review this change, go to: "
    <> volumeEdit [("page", "access"), partyq]
  NoticeVolumeAccess ->
    agent <> " set " <> party's <> " access to " <> TL.fromStrict (volumeAccessTitle perm msg) <> " on your volume, " <> volume <> ". To review this change, go to: "
    <> volumeEdit [("page", "access")]
  NoticeReleaseSlot ->
    agent <> " set the release level of a folder in " <> volume <> " to " <> TL.fromStrict (releaseTitle notificationRelease msg) <> ". To review this change, go to: "
    <> mailLink viewSlot (HTML, (volumeId <$> notificationVolume, slot)) []
  NoticeReleaseAsset ->
    agent <> " set the release level of a file in your volume (" <> volume <> ") to " <> TL.fromStrict (releaseTitle notificationRelease msg) <> ". To review this change, go to: "
    <> mailLink viewSlot (HTML, (volumeId <$> notificationVolume, slot)) [("asset", foldMap (BSC.pack . show) notificationAssetId)]
  NoticeReleaseExcerpt ->
    agent <> " set the release level of a highlight in your volume (" <> volume <> ") to " <> TL.fromStrict (releaseTitle notificationRelease msg) <> ". To review this change, go to: "
    <> assetSegment
  NoticeExcerptVolume ->
    agent <> " created a highlight in your volume (" <> volume <> "). To review this highlight, go to: "
    <> assetSegment
  NoticeCommentVolume ->
    agent <> " commented on your volume (" <> volume <> "). To review or reply, go to: "
    <> slotVolume [] <> "#comment-" <> foldMap (TL.pack . show) notificationCommentId
  NoticeCommentReply -> -- high risk information disclosure of volume name
    agent <> " replied to your comment on the volume, " <> volume <> ". To review or reply, go to: "
    <> slotVolume [] <> "#comment-" <> foldMap (TL.pack . show) notificationCommentId
  NoticeTagVolume ->
    agent <> " tagged the volume, " <> volume <> ", with \"" <> foldMap (TL.fromStrict . TE.decodeLatin1 . tagNameBS . tagName) notificationTag <> "\". To review tags, go to: "
    <> slotVolume [("tag", foldMap (tagNameBS . tagName) notificationTag)] <> "#panel-tags"
  NoticeSharedVolume ->
    agent <> " shared the following volume, " <> volume <> ", on Databrary. To review, go to: "
    <> mailLink viewVolume (HTML, maybe noId volumeId notificationVolume) []
  NoticeNewsletter ->
    "A new Databrary newsletter has been posted. Te see it, go to: http://databrary.org/news.html"
  where
  target = partyRow (accountParty notificationTarget)
  person p = on (/=) partyId p target ?> TL.fromStrict (partyName p)
  agent = fromMaybe "You" $ person notificationAgent
  partyp = person =<< notificationParty
  party = fromMaybe "you" partyp
  party'sOr your = maybe your (<> "'s") partyp
  party's = party'sOr "your"
  party'S = party'sOr "Your"
  personq p = ("party", maybe "" (BSC.pack . show . partyId) p)
  partyq = personq notificationParty
  partyEdit = partyEditLink mailLink target
  granted = maybe "revoked" (const "granted") notificationPermission
  volume = maybe "<VOLUME>" (TL.fromStrict . volumeName) notificationVolume
  volumeEdit = mailLink viewVolumeEdit (maybe noId volumeId notificationVolume)
  perm = fromMaybe PermissionNONE notificationPermission
  slot = Id $ SlotId (fromMaybe noId notificationContainerId) (fromMaybe fullSegment notificationSegment)
  assetSegment = mailLink viewAssetSegment (HTML, volumeId <$> notificationVolume, slot, fromMaybe noId notificationAssetId) []
  slotVolume
    | isJust notificationContainerId = mailLink viewSlot (HTML, (volumeId <$> notificationVolume, slot))
    | otherwise = mailLink viewVolume (HTML, maybe noId volumeId notificationVolume)

mailNotifications :: Messages -> [Notification] -> TL.Text
mailNotifications msg ~l@(Notification{ notificationTarget = u }:_) =
  TL.fromChunks ["Dear ", partyName target, ",\n"]
  <> foldMap (\n -> '\n' `TL.cons` mailNotification msg n `TL.snoc` '\n') l
  <> "\nYou can change your notification settings or unsubscribe here: "
  <> partyEditLink mailLink target target [("page", "notifications")] `TL.snoc` '\n'
  where
  target = partyRow (accountParty u)

htmlNotification :: Messages -> Notification -> H.Html
htmlNotification msg Notification{..} = case notificationNotice of
  NoticeAccountChange ->
    agent >> " changed " >> party's >> " "
    >> partyEdit (fromMaybe target notificationParty) [("page", "account")] "account information" >> "."
  NoticeAuthorizeRequest ->
    agent >> " requested "
    >> partyEdit target [("page", "apply"), partyq] "authorization" >> " from " >> party >> "."
  NoticeAuthorizeGranted ->
    "Your " >> partyEdit target [("page", "apply"), partyq] "authorization"
    >> " under " >> fromMaybe "yourself" (person =<< notificationParty) >> " has been " >> granted >> "."
  NoticeAuthorizeExpiring ->
    "Your " >> partyEdit target [("page", "apply"), partyq] "authorization" >> " through " >> party >> " will expire soon."
  NoticeAuthorizeExpired ->
    "Your " >> partyEdit target [("page", "apply"), partyq] "authorization" >> " through " >> party >> " is expired."
  NoticeAuthorizeChildRequest ->
    agent >> " requested "
    >> partyEdit (fromMaybe target notificationParty) [("page", "grant"), personq $ Just notificationAgent] "authorization" >> " from " >> party >> "."
  NoticeAuthorizeChildGranted ->
    agent >> " " >> granted >> " "
    >> partyEdit target [("page", "grant"), partyq] "authorization" >> " to " >> party >> "."
  NoticeAuthorizeChildExpiring ->
    party'S >> " " >> partyEdit target [("page", "grant"), partyq] "authorization" >> " will expire soon."
  NoticeAuthorizeChildExpired ->
    party'S >> " " >> partyEdit target [("page", "grant"), partyq] "authorization" >> " is expired."
  NoticeVolumeAssist ->
    agent >> " requested " >> volumeEdit [("page", "assist")] "assistance" >> " with " >> volume >> "."
  NoticeVolumeCreated ->
    agent >> " created " >> volume >> " on " >> party's >> " behalf."
  NoticeVolumeSharing ->
    agent >> " changed " >> volume >> " to "
    >> H.text (volumeAccessPresetTitle (PermissionNONE < perm) msg) >> "."
  NoticeVolumeAccessOther ->
    agent >> " set " >> party's >> " "
    >> volumeEdit [("page", "access"), partyq] "access" >> " to " >> H.text (volumeAccessTitle perm msg) >> " on " >> volume >> "."
  NoticeVolumeAccess ->
    agent >> " set " >> party's >> " "
    >> volumeEdit [("page", "access")] "access" >> " to " >> H.text (volumeAccessTitle perm msg) >> " on " >> volume >> "."
  NoticeReleaseSlot ->
    agent >> " set a " >> link viewSlot (HTML, (volumeId <$> notificationVolume, slot)) [] "folder"
    >> " in " >> volume >> " to " >> H.text (releaseTitle notificationRelease msg) >> "."
  NoticeReleaseAsset ->
    agent >> " set a " >> link viewSlot (HTML, (volumeId <$> notificationVolume, slot)) [("asset", foldMap (BSC.pack . show) notificationAssetId)] "file"
    >> " in " >> volume >> " to " >> H.text (releaseTitle notificationRelease msg) >> "."
  NoticeReleaseExcerpt ->
    agent >> " set a " >> assetSegment "highlight"
    >> " in " >> volume >> " to " >> H.text (releaseTitle notificationRelease msg) >> "."
  NoticeExcerptVolume ->
    agent >> " created a " >> assetSegment "highlight"
    >> " in " >> volume >> "."
  NoticeCommentVolume ->
    agent >> " " >> (slotVolume [] ("#comment-" <> foldMap (H.toValue . unId) notificationCommentId)) "commented"
    >> " on " >> volume >> "."
  NoticeCommentReply ->
    agent >> " " >> (slotVolume [] ("#comment-" <> foldMap (H.toValue . unId) notificationCommentId)) "replied"
    >> " to your comment on " >> volume >> "."
  NoticeTagVolume ->
    agent >> " " >> (slotVolume [("tag", foldMap (tagNameBS . tagName) notificationTag)] "#panel-tags") "tagged"
    >> " " >> volume >> " with " >> H.em (mapM_ (byteStringHtml . tagNameBS . tagName) notificationTag) >> "."
  NoticeSharedVolume ->
    agent >> " shared " >> volume >> "."
  NoticeNewsletter ->
    "A new " >> (H.a H.! HA.href "//databrary.org/news.html") "Databrary newsletter" >> " has been posted."
  where
  target = partyRow (accountParty notificationTarget)
  person p = on (/=) partyId p target ?> htmlPartyViewLink p ([] :: Query)
  agent = fromMaybe "You" $ person notificationAgent
  partyp = fmap (any (on (/=) partyId notificationAgent) notificationParty ?>) $ person =<< notificationParty
  party = maybe "you" (fromMaybe "themselves") partyp
  party'sOr your their = maybe your (maybe their (>> "'s")) partyp
  party's = party'sOr "your" "their own"
  party'S = party'sOr "Your" "Their own"
  personq p = ("party", maybe "" (BSC.pack . show . partyId) p)
  partyq = personq notificationParty
  link u a q h = H.a H.! actionLink u a (map (second Just) q :: Query) $ h
  partyEdit = partyEditLink link target
  granted = maybe "revoked" (const "granted") notificationPermission
  volume = maybe "<VOLUME>" (\v -> htmlVolumeViewLink v ([] :: Query)) notificationVolume
  volumeEdit = link viewVolumeEdit (maybe noId volumeId notificationVolume)
  perm = fromMaybe PermissionNONE notificationPermission
  slot = Id $ SlotId (fromMaybe noId notificationContainerId) (fromMaybe fullSegment notificationSegment)
  assetSegment = link viewAssetSegment (HTML, volumeId <$> notificationVolume, slot, fromMaybe noId notificationAssetId) []
  slotVolume q t = H.a H.! HA.href (if isJust notificationContainerId
    then actionValue viewSlot (HTML, (volumeId <$> notificationVolume, slot)) (q :: [(BSC.ByteString, BSC.ByteString)]) <> t
    else actionValue viewVolume (HTML, maybe noId volumeId notificationVolume) q <> t)

noId :: Num (IdType a) => Id a
noId = (Id $ -1)
