{-# LANGUAGE TemplateHaskell, QuasiQuotes, DataKinds, RecordWildCards, OverloadedStrings #-}
module Databrary.Model.Notification
  ( module Databrary.Model.Notification.Types
  , module Databrary.Model.Notification.Notify
  , addNotification
  , addBroadcastNotification
  , changeNotificationsDelivery
  , lookupUserNotifications
  , countUserNotifications
  , lookupUndeliveredNotifications
  , removeNotification
  , removeNotifications
  , cleanNotifications
  , removeMatchingNotifications
  , notificationJSON
  ) where

import Control.Monad (mfilter)
import Data.Int (Int64)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Database.PostgreSQL.Typed (pgSQL)

import Databrary.Has
import qualified Databrary.JSON as JSON
import Databrary.Service.DB
import Databrary.Model.SQL
import Databrary.Model.Id.Types
import Databrary.Model.Party
import Databrary.Model.Volume.Types
import Databrary.Model.Segment
import Databrary.Model.Tag.Types
import Databrary.Model.Notification.Types
import Databrary.Model.Notification.Notify
import Databrary.Model.Notification.SQL

useTDB

addNotification :: (MonadDB c m, MonadHas Party c m) => Notification -> m Notification
addNotification n@Notification{..} = do
  u <- peeks partyRow
  (i, t) <- dbQuery1' [pgSQL|INSERT INTO notification (target, notice, delivered, agent, party, volume, container, segment, asset, comment, tag, permission, release) VALUES (${partyId $ partyRow $ accountParty notificationTarget}, ${notificationNotice}, ${notificationDelivered}, ${partyId u}, ${partyId <$> notificationParty}, ${volumeId <$> notificationVolume}, ${notificationContainerId}, ${notificationSegment}, ${notificationAssetId}, ${notificationCommentId}, ${tagId <$> notificationTag}, ${notificationPermission}, ${notificationRelease}) RETURNING id, time|]
  return n
    { notificationId = i
    , notificationTime = t
    , notificationAgent = u
    }

addBroadcastNotification :: (MonadDB c m, MonadHas Party c m) => Notification -> m Int
addBroadcastNotification Notification{..} = do
  u <- peeks (partyId . partyRow)
  dbExecute [pgSQL|INSERT INTO notification (target, notice, delivered, agent, party, volume, container, segment, asset, comment, tag, permission, release) SELECT target, notice, ${notificationDelivered}, ${u}, ${partyId <$> notificationParty}, ${volumeId <$> notificationVolume}, ${notificationContainerId}, ${notificationSegment}, ${notificationAssetId}, ${notificationCommentId}, ${tagId <$> notificationTag}, ${notificationPermission}, ${notificationRelease} FROM notify_view WHERE notice = ${notificationNotice} AND delivery > 'none' AND target <> ${u}|]

changeNotificationsDelivery :: MonadDB c m => [Notification] -> Delivery -> m Int
changeNotificationsDelivery nl d =
  dbExecute [pgSQL|UPDATE notification SET delivered = ${d} WHERE id = ANY (${map notificationId nl}) AND delivered < ${d}|]

lookupUserNotifications :: (MonadDB c m, MonadHas Account c m) => m [Notification]
lookupUserNotifications = do
  u <- peek
  dbQuery $ ($ u) <$> $(selectQuery selectTargetNotification "$WHERE target = ${view u :: Id Party} ORDER BY notification.id DESC")

countUserNotifications :: (MonadDB c m, MonadHas (Id Party) c m) => m Int64
countUserNotifications = do
  u <- peek
  dbQuery1' $ fromMaybe 0 <$> [pgSQL|$SELECT count(id) FROM notification WHERE target = ${u :: Id Party} AND delivered = 'none'|]

lookupUndeliveredNotifications :: MonadDB c m => Delivery -> m [Notification]
lookupUndeliveredNotifications d =
  dbQuery $(selectQuery selectNotification "JOIN notify_view USING (target, notice) WHERE delivery >= ${d} AND delivered = 'none' ORDER BY notification.target, notification.id")

removeNotification :: (MonadDB c m, MonadHas (Id Party) c m) => Id Notification -> m Bool
removeNotification i = do
  p <- peek
  dbExecute1 [pgSQL|DELETE FROM notification WHERE id = ${i} AND target = ${p :: Id Party}|]

removeNotifications :: (MonadDB c m, MonadHas (Id Party) c m) => m Int
removeNotifications = do
  p <- peek
  dbExecute [pgSQL|DELETE FROM notification WHERE target = ${p :: Id Party} AND agent <> ${partyId $ partyRow nobodyParty}|]

cleanNotifications :: MonadDB c m => m Int
cleanNotifications =
  dbExecute [pgSQL|DELETE FROM notification WHERE delivered > 'none' AND time < CURRENT_TIMESTAMP - interval '30 days'|]

removeMatchingNotifications :: MonadDB c m => Notification -> m Int
removeMatchingNotifications Notification{..} =
  dbExecute [pgSQL|DELETE FROM notification
    WHERE notice = ${notificationNotice}
      AND target = COALESCE(${mfilter (no /=) $ Just $ partyId $ partyRow $ accountParty notificationTarget}, target) 
      AND agent = COALESCE(${mfilter (no /=) $ Just $ partyId notificationAgent}, agent) 
      AND COALESCE(party, -1) = COALESCE(${partyId <$> notificationParty}, party, -1)
      AND COALESCE(volume, -1) = COALESCE(${volumeId <$> notificationVolume}, volume, -1)
      AND COALESCE(container, -1) = COALESCE(${notificationContainerId}, container, -1)
      AND COALESCE(segment, 'empty') <@ ${fromMaybe fullSegment notificationSegment} 
      AND COALESCE(asset, -1) = COALESCE(${notificationAssetId}, asset, -1)
      AND COALESCE(comment, -1) = COALESCE(${notificationCommentId}, comment, -1)
      AND COALESCE(tag, -1) = COALESCE(${tagId <$> notificationTag}, tag, -1)
    |]
  where
  no :: Num (IdType a) => Id a
  no = Id $ -1

notificationJSON :: JSON.ToNestedObject o u => Notification -> JSON.Record (Id Notification) o
notificationJSON Notification{..} = JSON.Record notificationId $
     "notice" JSON..= notificationNotice
  <> "time" JSON..= notificationTime
  <> "delivered" JSON..= notificationDelivered
  <> "agent" JSON..=. JSON.recordObject ({-on (==) partyId notificationAgent (partyRow (accountParty notificationTarget)) ?!>-} partyRowJSON notificationAgent)
  <> "party" JSON..=? (partyId <$> notificationParty)
  <> "permission" JSON..=? notificationPermission
  <> "volume" JSON..=? (volumeId <$> notificationVolume)
  <> "container" JSON..=? notificationContainerId
  <> "segment" JSON..=? notificationSegment
  <> "asset" JSON..=? notificationAssetId
  <> "comment" JSON..=? notificationCommentId
  <> "tag" JSON..=? (tagName <$> notificationTag)
