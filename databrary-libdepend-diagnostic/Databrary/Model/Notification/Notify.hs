{-# LANGUAGE TemplateHaskell, QuasiQuotes, DataKinds #-}
module Databrary.Model.Notification.Notify
  ( lookupNotify
  , lookupAccountNotify
  , NoticeMap
  , changeNotify
  , removeNotify
  , lookupNoticePartyAuthorization
  ) where

import Control.Monad (when)
import qualified Data.Aeson as JSON
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import Database.PostgreSQL.Typed.Query (pgSQL)

import Databrary.Has (peek)
import Databrary.Service.DB
import Databrary.Model.SQL
import Databrary.Model.Id
import Databrary.Model.Permission
import Databrary.Model.Party.Types
import Databrary.Model.Identity.Types
import Databrary.Model.Notification.Notice
import Databrary.Model.Notification.SQL

useTDB

lookupNotify :: MonadDB c m => Account -> Notice -> m Delivery
lookupNotify a n = fromMaybeDelivery <$>
  dbQuery1 $(selectQuery selectNotifyDelivery "$WHERE target = ${partyId $ partyRow $ accountParty a} AND notice = ${n}")

lookupAccountNotify :: MonadDB c m => Account -> m (NoticeMap Delivery)
lookupAccountNotify a = NoticeMap <$>
  dbQuery [pgSQL|!SELECT notice, delivery FROM notify_view WHERE target = ${partyId $ partyRow $ accountParty a} ORDER BY notice|]

changeNotify :: MonadDB c m => Account -> Notice -> Delivery -> m ()
changeNotify a n d = do
  (r, _) <- updateOrInsert
    [pgSQL|UPDATE notify SET delivery = ${d} WHERE target = ${partyId $ partyRow $ accountParty a} AND notice = ${n}|]
    [pgSQL|INSERT INTO notify (target, notice, delivery) VALUES (${partyId $ partyRow $ accountParty a}, ${n}, ${d})|]
  when (r /= 1) $ fail $ "changeNotify: " ++ show r ++ " rows"

-- |This resets to the default value (not necessarily DeliveryNone).
removeNotify :: MonadDB c m => Account -> Notice -> m Bool
removeNotify a n =
  dbExecute1 [pgSQL|DELETE FROM notify WHERE target = ${partyId $ partyRow $ accountParty a} AND notice = ${n}|]

lookupNoticePartyAuthorization :: (MonadHasIdentity c m, MonadDB c m) => Notice -> m [(Party, Maybe Permission, Delivery)]
lookupNoticePartyAuthorization n = do
  ident <- peek
  dbQuery $(selectQuery (selectPartyAuthorizationNotify 'ident) "WHERE notice = ${n} AND account.password IS NOT NULL")

newtype NoticeMap a = NoticeMap [(Notice, a)]

noticeInt :: Notice -> Int
noticeInt = fromIntegral . unId . noticeId

noticeMapToList :: NoticeMap a -> [Maybe a]
noticeMapToList (NoticeMap m) = pop 0 m where
  pop _ [] = []
  pop i nl@((n,x):l) = case i `compare` noticeInt n of
    LT -> Nothing : pop (succ i) nl
    EQ -> Just x : pop (succ i) l
    GT -> error "NoticeMap: out of order"

instance JSON.ToJSON a => JSON.ToJSON (NoticeMap a) where
  toJSON (NoticeMap m) = JSON.Array $ V.create $ do
    v <- VM.replicate (succ (noticeInt maxBound)) JSON.Null
    mapM_ (\(n,x) -> VM.write v (noticeInt n) $ JSON.toJSON x) m
    return v
  toEncoding = JSON.foldable . noticeMapToList
