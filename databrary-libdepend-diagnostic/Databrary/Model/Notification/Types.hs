{-# LANGUAGE TypeFamilies, OverloadedStrings #-}
module Databrary.Model.Notification.Types
  ( module Databrary.Model.Notification.Notice
  , Notification(..)
  , blankNotification
  ) where

import Databrary.Model.Time
import Databrary.Model.Id.Types
import Databrary.Model.Kind
import Databrary.Model.Party
import Databrary.Model.Volume.Types
import Databrary.Model.Container.Types
import Databrary.Model.Segment
import Databrary.Model.Asset.Types
import Databrary.Model.Comment.Types
import Databrary.Model.Tag.Types
import Databrary.Model.Permission.Types
import Databrary.Model.Release.Types
import Databrary.Model.Notification.Notice

type instance IdType Notification = Int32

data Notification = Notification
  { notificationId :: Id Notification
  , notificationTarget :: !Account
  , notificationNotice :: !Notice
  , notificationTime :: Timestamp
  , notificationDelivered :: !Delivery
  , notificationAgent :: PartyRow
  , notificationParty :: Maybe PartyRow
  , notificationVolume :: Maybe VolumeRow
  , notificationPermission :: Maybe Permission
  , notificationContainerId :: Maybe (Id Container)
  , notificationSegment :: Maybe Segment
  , notificationAssetId :: Maybe (Id Asset)
  , notificationRelease :: Maybe Release
  , notificationCommentId :: Maybe (Id Comment)
  , notificationTag :: Maybe Tag
  }

instance Kinded Notification where
  kindOf _ = "notification"

blankNotification :: Account -> Notice -> Notification
blankNotification target notice = Notification
  { notificationId = error "blankNotification"
  , notificationTarget = target
  , notificationNotice = notice
  , notificationTime = error "blankNotification"
  , notificationDelivered = DeliveryNone
  , notificationAgent = partyRow nobodyParty
  , notificationParty = Nothing
  , notificationVolume = Nothing
  , notificationPermission = Nothing
  , notificationContainerId = Nothing
  , notificationSegment = Nothing
  , notificationAssetId = Nothing
  , notificationRelease = Nothing
  , notificationCommentId = Nothing
  , notificationTag = Nothing
  }
