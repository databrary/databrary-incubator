{-# LANGUAGE OverloadedStrings, TemplateHaskell, RecordWildCards, DataKinds #-}
module Databrary.Model.Activity
  ( lookupPartyActivity
  , lookupVolumeActivity
  , lookupContainerActivity
  , activityJSON
  ) where

import Control.Applicative ((<|>))
import Control.Arrow ((&&&))
import Control.Monad (forM)
import qualified Data.ByteString.Char8 as BSC
import Data.Function (on)
import qualified Data.HashMap.Strict as HM
import Data.List (foldl')
import qualified Data.Map as Map
import Data.Monoid ((<>))
import qualified Data.Text as T
import Data.Time.Clock (diffUTCTime)

import Databrary.Ops
import Databrary.Has
import qualified Databrary.JSON as JSON
import Databrary.Service.DB
import Databrary.Model.SQL
import Databrary.Model.Identity
import Databrary.Model.Id
import Databrary.Model.Time
import Databrary.Model.Audit
import Databrary.Model.Volume
import Databrary.Model.VolumeAccess
import Databrary.Model.Party
import Databrary.Model.Authorize
import Databrary.Model.Container
import Databrary.Model.Segment
import Databrary.Model.Slot
import Databrary.Model.Asset
import Databrary.Model.AssetRevision
import Databrary.Model.Activity.Types
import Databrary.Model.Activity.SQL

onActivityTime :: (Timestamp -> Timestamp -> a) -> Activity -> Activity -> a
onActivityTime = (`on` auditWhen . activityAudit)

orderActivity :: Activity -> Activity -> Ordering
orderActivity = onActivityTime compare

mergeActivity :: [Activity] -> [Activity] -> [Activity]
mergeActivity = mergeBy $ \x y -> orderActivity x y <> LT

mergeActivities :: [[Activity]] -> [Activity]
mergeActivities = foldr1 mergeActivity

joinActivitiesWith :: (Activity -> Maybe (Activity -> Maybe Activity)) -> [Activity] -> [Activity]
joinActivitiesWith f (a1:a1r) = maybe al
  (\af ->
    let la c (a2:a2r)
          | onActivityTime diffUTCTime a2 a1 >= 1 = al
          | ((==) `on` auditIdentity . activityAudit) a1 a2, Just a <- af a2 = a : joinActivitiesWith f (c a2r)
          | otherwise = la (c . (a2 :)) a2r
        la c [] = a1 : joinActivitiesWith f (c [])
    in la id a1r)
  $ f a1
  where al = a1 : joinActivitiesWith f a1r
joinActivitiesWith _ [] = []

chainPrev :: Ord a => (ActivityTarget -> a) -> [Activity] -> [Activity]
chainPrev f = scan Map.empty where
  scan m (a@Activity{ activityAudit = Audit{ auditAction = act }, activityTarget = t }:l) = a{ activityPrev = p } : scan m' l where
    (p, m') = case act of
      AuditActionAdd -> (Nothing, Map.insert (f t) t m)
      AuditActionRemove -> Map.updateLookupWithKey (\_ _ -> Nothing) (f t) m
      AuditActionChange -> Map.insertLookupWithKey (const const) (f t) t m
      _ -> (activityPrev a, m)
  scan _ [] = []

maskPasswords :: [Activity] -> [Activity]
maskPasswords = mp HM.empty (0 :: Integer) where
  -- this could be done much more simply since passwords are never going to repeat in practice
  mp m c (a@Activity{ activityTarget = at@ActivityAccount{ activityAccountPassword = Just p } }:l)
    | Just i <- HM.lookup p m = f i : mp m c l
    | otherwise = f c : mp (HM.insert p c m) (succ c) l
    where f i = a{ activityTarget = at{ activityAccountPassword = Just $ BSC.pack $ show i } }
  mp m c (a:l) = a : mp m c l
  mp _ _ [] = []

lookupPartyActivity :: (MonadDB c m, MonadHasIdentity c m) => Party -> m [Activity]
lookupPartyActivity p = do
  ident <- peek
  pa <- chainPrev (const ())
    <$> dbQuery $(selectQuery selectActivityParty $ "WHERE party.id = ${partyId $ partyRow p} AND " ++ activityQual)
  ca <- chainPrev (const ()) . maskPasswords
    <$> dbQuery $(selectQuery selectActivityAccount $ "WHERE account.id = ${partyId $ partyRow p} ORDER BY audit_time") -- unqual: include logins
  aa <- chainPrev (partyId . partyRow . authorizeChild . authorization . activityAuthorize)
    <$> dbQuery $(selectQuery (selectActivityAuthorize 'p 'ident) $ "WHERE " ++ activityQual)
  return $ mergeActivities [pa, ca, aa]

lookupVolumeActivity :: (MonadDB c m, MonadHasIdentity c m) => Volume -> m [Activity]
lookupVolumeActivity vol = do
  ident <- peek
  va <- chainPrev (const ())
    <$> dbQuery $(selectQuery selectActivityVolume $ "!WHERE volume.id = ${volumeId $ volumeRow vol} AND " ++ activityQual)
  aa <- chainPrev (partyId . partyRow . volumeAccessParty . activityAccess)
    <$> dbQuery $(selectQuery (selectActivityAccess 'vol 'ident) $ "WHERE " ++ activityQual)
  return $ mergeActivities [va, aa]

addAssetRevision :: (MonadDB c m, MonadHasIdentity c m) => Volume -> Activity -> m Activity
addAssetRevision vol
  act@Activity{ activityAudit = Audit{ auditAction = aa }, activityTarget = ActivityAssetSlot{ activityAssetId = ai }, activityPrev = Nothing }
  | aa <= AuditActionChange = do
    ar <- if aa == AuditActionChange then lookupAssetReplace a else return Nothing
    at <- lookupAssetTranscode a
    return act
      { activityReplace = revisionOrig <$> ar
      , activityTranscode = revisionOrig <$> at
      }
    where
    a = ba{ assetRow = (assetRow ba){ assetId = ai } }
    ba = blankAsset vol
addAssetRevision _ a = return a

mergeAssetCreation :: [Activity] -> [Activity]
mergeAssetCreation = joinActivitiesWith f1 where
  f1 a@Activity{ activityAudit = Audit{ auditAction = AuditActionAdd  }, activityTarget = ActivityAsset aa, activityPrev = Nothing } = Just f2 where
    f2 Activity{ activityAudit = Audit{ auditAction = AuditActionChange }, activityTarget = ActivityAsset ac, activityPrev = Just (ActivityAsset aa') }
      | assetId aa == assetId aa' = Just a{ activityTarget = ActivityAsset ac, activityPrev = Just (ActivityAsset aa) }
    f2 _ = Nothing
  f1 _ = Nothing

mergeActivityAssetAndSlot :: ActivityTarget -> ActivityTarget -> Maybe ActivityTarget
mergeActivityAssetAndSlot (ActivityAsset ar) (ActivityAssetSlot ai si) =
  assetId ar == ai ?> ActivityAssetAndSlot ar si
mergeActivityAssetAndSlot _ _ = Nothing

mergeAssetAndSlot :: [Activity] -> [Activity]
mergeAssetAndSlot = joinActivitiesWith f1 where
  f1 Activity{ activityAudit = a1, activityTarget = t1, activityPrev = p1, activityReplace = Nothing, activityTranscode = Nothing } = Just f2 where
    f2 a@Activity{ activityAudit = a2, activityTarget = t2, activityPrev = p2 }
      | auditAction a1 <= auditAction a2 && auditAction a2 <= AuditActionChange
      , Just t <- mergeActivityAssetAndSlot t1 t2 = Just a
        { activityAudit = a1
        , activityTarget = t
        , activityPrev = (do
          p1t <- p1
          mergeActivityAssetAndSlot p1t =<< p2) <|> p1 <|> p2
        }
    f2 _ = Nothing
  f1 _ = Nothing

lookupContainerActivity :: (MonadDB c m, MonadHasIdentity c m) => Container -> m [Activity]
lookupContainerActivity cont = do
  ca <- chainPrev (const ())
    <$> dbQuery $(selectQuery selectActivityContainer $ "WHERE container.id = ${containerId $ containerRow cont} AND " ++ activityQual)
  ra <- chainPrev (slotSegmentId . activitySlotId)
    <$> dbQuery $(selectQuery selectActivityRelease $ "WHERE slot_release.container = ${containerId $ containerRow cont} AND " ++ activityQual)

  asa <- mapM (addAssetRevision (containerVolume cont)) =<< chainPrev activityAssetId
    <$> dbQuery $(selectQuery selectActivityAssetSlot $ "WHERE slot_asset.container = ${containerId $ containerRow cont} AND " ++ activityQual)

  caa <- mergeAssetCreation . chainPrev (assetId . activityAssetRow)
    <$> dbQuery $(selectQuery selectActivityAsset $ "JOIN slot_asset ON asset.id = slot_asset.asset WHERE slot_asset.container = ${containerId $ containerRow cont} AND " ++ activityQual)
  let uam m Activity{ activityAudit = Audit{ auditAction = AuditActionRemove, auditWhen = t }, activityTarget = ActivityAssetSlot{ activityAssetId = a } } =
        Map.insert a t m
      uam m Activity{ activityAudit = Audit{ auditAction = AuditActionChange, auditWhen = t }, activityReplace = Just ar } =
        Map.insert (assetId $ assetRow ar) t m
      uam m _ = m
      dam = flip $ Map.delete . assetId . activityAssetRow . activityTarget
      oal = Map.toList $ foldl' dam (foldl' uam Map.empty asa) caa
  oaa <- forM oal $ \(ai, at) ->
    mergeAssetCreation . chainPrev (const ())
      <$> dbQuery $(selectQuery selectActivityAsset $ "WHERE asset.id = ${ai} AND audit_time <= ${at} AND " ++ activityQual)

  cea <- chainPrev (activityAssetId &&& activitySegment)
    <$> dbQuery $(selectQuery selectActivityExcerpt $ "JOIN slot_asset ON excerpt.asset = slot_asset.asset WHERE slot_asset.container = ${containerId $ containerRow cont} AND " ++ activityQual)

  return $ mergeAssetAndSlot $ mergeActivities (ca:ra:asa:cea:caa:oaa)

-- EDIT permission assumed for all
activityTargetJSON :: ActivityTarget -> (T.Text, JSON.Object, JSON.Object)
activityTargetJSON (ActivityParty p) =
  ("party", mempty, JSON.recordObject $
    partyRowJSON p)
activityTargetJSON ActivityAccount{..} =
  ("account", mempty,
    "email" JSON..= activityAccountEmail <> "password" JSON..= activityAccountPassword)
activityTargetJSON (ActivityAuthorize a) =
  ("authorize", "party" JSON..=: partyJSON (authorizeChild $ authorization a),
    authorizeJSON a)
activityTargetJSON (ActivityVolume v) =
  ("volume", mempty, JSON.recordObject $
    volumeRowJSON v JSON..<>
      "alias" JSON..=? volumeAlias v)
activityTargetJSON (ActivityAccess a) =
  ("access", "party" JSON..=: partyJSON (volumeAccessParty a),
    volumeAccessJSON a)
activityTargetJSON (ActivityContainer c) =
  ("container", mempty, JSON.recordObject $
    containerRowJSON c JSON..<>
      "date" JSON..=? containerDate c)
activityTargetJSON ActivityRelease{..} =
  ("release", segmentJSON $ slotSegmentId activitySlotId,
    "release" JSON..= activityRelease)
activityTargetJSON (ActivityAsset a) =
  ("asset", "id" JSON..= assetId a,
    "classification" JSON..=? assetRelease a <> "name" JSON..=? assetName a)
activityTargetJSON (ActivityAssetSlot a s) =
  ("asset", "id" JSON..= a,
    segmentJSON $ slotSegmentId s)
activityTargetJSON (ActivityAssetAndSlot a s) = (n, i, o <> segmentJSON (slotSegmentId s)) where
  (n, i, o) = activityTargetJSON (ActivityAsset a)
activityTargetJSON ActivityExcerpt{..} =
  ("excerpt", "id" JSON..= activityAssetId <> segmentJSON activitySegment,
    "excerpt" JSON..=? activityExcerptRelease)

activityAssetJSON :: Asset -> JSON.Object
activityAssetJSON a = JSON.recordObject $ assetJSON a JSON..<> "name" JSON..=? assetName (assetRow a)

activityJSON :: Activity -> Maybe JSON.Object
activityJSON Activity{ activityAudit = Audit{..}, ..} = auditAction == AuditActionChange && HM.null new && HM.null old ?!>
  new <> key
    <> "when" JSON..= auditWhen
    <> "action" JSON..= show (auditAction)
    <> "ip" JSON..= show (auditIp auditIdentity)
    <> "user" JSON..= auditWho auditIdentity
    <> "type" JSON..= typ
    <> "old" JSON..=? (old <!? HM.null old)
    <> "replace" JSON..=? (activityAssetJSON <$> activityReplace)
    <> "transcode" JSON..=? (activityAssetJSON <$> activityTranscode)
  where
  (new, old)
    | auditAction == AuditActionRemove
      = (HM.empty, targ)
    | Just p <- activityPrev
    , (_, _, prev) <- activityTargetJSON p
    , int <- HM.filter id $ HM.intersectionWith (==) targ prev
      = (if auditAction == AuditActionAdd then targ else HM.difference targ int, HM.difference prev int)
    | otherwise
      = (targ, HM.empty)
  (typ, key, targ) = activityTargetJSON activityTarget
