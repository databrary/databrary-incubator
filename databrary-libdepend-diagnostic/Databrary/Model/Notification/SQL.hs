{-# LANGUAGE TemplateHaskell #-}
module Databrary.Model.Notification.SQL
  ( selectNotifyDelivery
  , selectTargetNotification
  , selectNotification
  , selectPartyAuthorizationNotify
  ) where

import qualified Language.Haskell.TH as TH

import Databrary.Has
import Databrary.Model.SQL.Select
import Databrary.Model.Time
import Databrary.Model.Id.Types
import Databrary.Model.Permission.Types
import Databrary.Model.Release.Types
import Databrary.Model.Party.Types
import Databrary.Model.Party.SQL
import Databrary.Model.Volume.Types
import Databrary.Model.Volume.SQL
import Databrary.Model.Container.Types
import Databrary.Model.Segment
import Databrary.Model.Asset.Types
import Databrary.Model.Tag.Types
import Databrary.Model.Tag.SQL
import Databrary.Model.Comment.Types
import Databrary.Model.Notification.Types

selectNotifyDelivery :: Selector
selectNotifyDelivery = selectMap (TH.VarE 'fromMaybeDelivery `TH.AppE`) $ selectColumn "notify_view" "delivery"

makePartyAuthorizationNotice :: (Party, Maybe Permission) -> Delivery -> (Party, Maybe Permission, Delivery)
makePartyAuthorizationNotice (p, a) d = (p, a, d)

selectPartyAuthorizationNotify :: TH.Name -- ^ 'Identity'
  -> Selector -- ^ @('Party', Maybe 'Permission', 'Delivery')@
selectPartyAuthorizationNotify ident = selectJoin 'makePartyAuthorizationNotice
  [ selectPartyAuthorization ident
  , joinOn "id = target"
    selectNotifyDelivery
  ]

makeNotification :: Id Notification -> Notice -> Timestamp -> Delivery -> Maybe Permission -> Maybe (Id Container) -> Maybe Segment -> Maybe (Id Asset) -> Maybe Release -> Maybe (Id Comment) -> PartyRow -> Maybe PartyRow -> Maybe VolumeRow -> Maybe Tag -> Account -> Notification
makeNotification i n t d e c s a r m w p v g u = Notification i (view u) n t d w p v e c s a r m g

notificationRow :: Selector -- ^ @'PartyRow' -> Maybe 'PartyRow' -> Maybe 'VolumeRow' -> Maybe 'Tag' -> 'Account' -> 'Notification'@
notificationRow = selectColumns 'makeNotification "notification" ["id", "notice", "time", "delivered", "permission", "container", "segment", "asset", "release", "comment"]

selectTargetNotification :: Selector -- ^ @'Account' -> 'Notification'@
selectTargetNotification = selectJoin '($)
  [ notificationRow
  , joinOn "notification.agent = agent.id"
    $ (selectColumns 'PartyRow "party" ["id", "name", "prename", "orcid", "affiliation", "url"]) `fromAlias` "agent"
  , maybeJoinOn "notification.party = nparty.id"
    $ (selectColumns 'PartyRow "party" ["id", "name", "prename", "orcid", "affiliation", "url"]) `fromAlias` "nparty"
  , maybeJoinOn "notification.volume = volume.id"
    $ selectVolumeRow
  , maybeJoinOn "notification.tag = tag.id"
    $ selectColumns 'Tag "tag" ["id", "name"] -- selectTag
  ]

selectNotification :: Selector -- ^ @'Notification'@
selectNotification = selectJoin '($)
  [ selectTargetNotification
  , joinOn "notification.target = account.id"
    selectUserAccount
  ]
