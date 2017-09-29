{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module Databrary.Controller.Notification
  ( viewNotify
  , postNotify
  , createNotification
  , createVolumeNotification
  , broadcastNotification
  , viewNotifications
  , deleteNotification
  , deleteNotifications
  , forkNotifier
  , updateStateNotifications
  , updateAuthorizeNotifications
  ) where

import Control.Applicative ((<|>))
import Control.Concurrent (ThreadId, forkFinally, threadDelay)
import Control.Concurrent.MVar (takeMVar, tryTakeMVar)
import Control.Monad (join, when, void, forM_)
import Data.Function (on)
import Data.List (groupBy)
import Data.Time.Clock (getCurrentTime, addUTCTime)
import Database.PostgreSQL.Typed (pgSQL)
import Network.HTTP.Types (noContent204)
import qualified Text.Regex.Posix as Regex

import Databrary.Has
import Databrary.Ops
import qualified Databrary.JSON as JSON
import Databrary.Service.Types
import Databrary.Service.DB
import Databrary.Service.Notification
import Databrary.Service.Log
import Databrary.Service.Mail
import Databrary.Service.Messages
import Databrary.Context
import Databrary.Model.Id.Types
import Databrary.Model.Party
import Databrary.Model.Volume.Types
import Databrary.Model.Authorize.Types
import Databrary.Model.Notification
import Databrary.HTTP.Path.Parser
import Databrary.HTTP.Form.Deform
import Databrary.Controller.Permission
import Databrary.Controller.Paths
import Databrary.Controller.Form
import Databrary.Action
import Databrary.View.Notification

viewNotify :: ActionRoute ()
viewNotify = action GET (pathJSON </< "notify") $ \() -> withAuth $ do
  u <- authAccount
  n <- lookupAccountNotify u
  return $ okResponse [] $ JSON.toEncoding n

postNotify :: ActionRoute ()
postNotify = action POST (pathJSON </< "notify") $ \() -> withAuth $ do
  u <- authAccount
  (nl, md) <- runForm Nothing $ do
    csrfForm
    (,)
      <$> ("notice" .:> return <$> deform <|> withSubDeforms (const deform))
      <*> ("delivery" .:> deformNonEmpty deform)
  mapM_ (maybe (void . removeNotify u) (\d n -> changeNotify u n d) md) nl
  return $ emptyResponse noContent204 []

createNotification :: Notification -> ActionM ()
createNotification n' = do
  d <- lookupNotify (notificationTarget n') (notificationNotice n')
  when (d > DeliveryNone) $ do
    n <- addNotification n'
    if notificationDelivered n' >= DeliveryAsync
      then sendTargetNotifications [n]
      else when (d >= DeliveryAsync) $ focusIO $ triggerNotifications Nothing

broadcastNotification :: Bool -> ((Notice -> Notification) -> Notification) -> ActionM ()
broadcastNotification add f =
  void $ (if add then addBroadcastNotification else removeMatchingNotifications) $ f $ blankNotification $ siteAccount nobodySiteAuth

createVolumeNotification :: Volume -> ((Notice -> Notification) -> Notification) -> ActionM ()
createVolumeNotification v f = do
  u <- peek
  forM_ (volumeOwners v) $ \(p, _) -> when (u /= p) $
    createNotification (f $ blankNotification blankAccount{ accountParty = blankParty{ partyRow = (partyRow blankParty){ partyId = p } } })
      { notificationVolume = Just $ volumeRow v }

viewNotifications :: ActionRoute ()
viewNotifications = action GET (pathJSON </< "notification") $ \() -> withAuth $ do
  _ <- authAccount
  nl <- lookupUserNotifications
  _ <- changeNotificationsDelivery (filter ((DeliverySite >) . notificationDelivered) nl) DeliverySite -- would be nice if it could be done as part of lookupNotifications
  msg <- peek
  return $ okResponse [] $ JSON.mapRecords (\n -> notificationJSON n JSON..<> "html" JSON..= htmlNotification msg n) nl

deleteNotification :: ActionRoute (Id Notification)
deleteNotification = action DELETE (pathJSON >/> pathId) $ \i -> withAuth $ do
  _ <- authAccount
  r <- removeNotification i
  if r
    then return $ emptyResponse noContent204 []
    else peeks notFoundResponse

deleteNotifications :: ActionRoute ()
deleteNotifications = action DELETE (pathJSON >/> "notification") $ \() -> withAuth $ do
  _ <- authAccount
  _ <- removeNotifications
  return $ emptyResponse noContent204 []

-- |Assumed to be all same target
sendTargetNotifications :: (MonadMail c m, MonadHas Notifications c m, MonadHas Messages c m) => [Notification] -> m ()
sendTargetNotifications l@(Notification{ notificationTarget = u }:_) = do
  Notifications{ notificationsFilter = filt, notificationsCopy = copy } <- peek
  msg <- peek
  sendMail (map Right (filter (Regex.matchTest filt . accountEmail) [u])) (maybe [] (return . Left) copy)
    "Databrary notifications"
    $ mailNotifications msg l
sendTargetNotifications [] = return ()

emitNotifications :: Delivery -> ContextM ()
emitNotifications d = do
  unl <- lookupUndeliveredNotifications d
  mapM_ sendTargetNotifications $ groupBy ((==) `on` partyId . partyRow . accountParty . notificationTarget) unl
  _ <- changeNotificationsDelivery unl d
  return ()
  
runNotifier :: Service -> IO ()
runNotifier rc = loop where
  t = notificationsTrigger $ serviceNotification rc
  loop = do
    d' <- takeMVar t
    d <- d' `orElseM` do
      threadDelay 60000000 -- 60 second throttle on async notifications
      join <$> tryTakeMVar t
    runContextM (emitNotifications $ periodicDelivery d) rc
    loop

forkNotifier :: Service -> IO ThreadId
forkNotifier rc = forkFinally (runNotifier rc) $ \r -> do
  t <- getCurrentTime
  logMsg t ("notifier aborted: " ++ show r) (view rc)

updateStateNotifications :: MonadDB c m => m ()
updateStateNotifications =
  dbTransaction $ dbExecute_ [pgSQL|#
    CREATE TEMPORARY TABLE notification_authorize_expire (id, target, party, permission, time, notice) ON COMMIT DROP
      AS   WITH authorize_expire AS (SELECT * FROM authorize WHERE expires BETWEEN CURRENT_TIMESTAMP - interval '30 days' AND CURRENT_TIMESTAMP + interval '1 week')
         SELECT notification.id, COALESCE(child, target), COALESCE(parent, party), site, expires, CASE WHEN expires <= CURRENT_TIMESTAMP THEN ${NoticeAuthorizeExpired} WHEN expires > CURRENT_TIMESTAMP THEN ${NoticeAuthorizeExpiring} END
           FROM notification FULL JOIN authorize_expire JOIN account ON child = id ON child = target AND parent = party
          WHERE (notice IS NULL OR notice = ${NoticeAuthorizeExpiring} OR notice = ${NoticeAuthorizeExpired})
      UNION ALL
         SELECT notification.id, COALESCE(parent, target), COALESCE(child, party), site, expires, CASE WHEN expires <= CURRENT_TIMESTAMP THEN ${NoticeAuthorizeChildExpired} WHEN expires > CURRENT_TIMESTAMP THEN ${NoticeAuthorizeChildExpiring} END
           FROM notification FULL JOIN authorize_expire JOIN account ON parent = id ON parent = target AND child = party
          WHERE (notice IS NULL OR notice = ${NoticeAuthorizeChildExpiring} OR notice = ${NoticeAuthorizeChildExpired});
    DELETE FROM notification USING notification_authorize_expire nae
          WHERE notification.id = nae.id AND nae.notice IS NULL;
    UPDATE notification SET notice = nae.notice, time = nae.time, delivered = CASE WHEN notification.notice = nae.notice THEN delivered ELSE 'none' END, permission = nae.permission
      FROM notification_authorize_expire nae WHERE notification.id = nae.id;
    INSERT INTO notification (notice, target, party, permission, time, agent)
         SELECT notice, target, party, permission, time, ${partyId $ partyRow nobodyParty}
           FROM notification_authorize_expire WHERE id IS NULL;
  |]

updateAuthorizeNotifications :: (MonadHas Context c m, MonadDB c m) => Maybe Authorize -> Authorize -> m ()
updateAuthorizeNotifications Nothing _ = return ()
updateAuthorizeNotifications (Just Authorize{ authorizeExpires = o }) Authorize{ authorization = Authorization{ authorizeChild = Party{ partyRow = PartyRow{ partyId = c } }, authorizeParent = Party{ partyRow = PartyRow{ partyId = p } } }, authorizeExpires = e } = do
  t <- peeks contextTimestamp
  let t' = addUTCTime 691200 t
  when (all (t' <) e && any (t' >=) o) $
    dbExecute_ [pgSQL|#
      DELETE FROM notification
       WHERE agent = ${partyId $ partyRow nobodyParty}
         AND (((notice = ${NoticeAuthorizeExpiring} OR notice = ${NoticeAuthorizeExpired}) AND target = ${c} AND party = ${p})
           OR ((notice = ${NoticeAuthorizeChildExpiring} OR notice = ${NoticeAuthorizeChildExpired}) AND target = ${p} AND party = ${c}))
    |]
