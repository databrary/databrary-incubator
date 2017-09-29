{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes, RecordWildCards, DataKinds #-}
module Databrary.Model.Party
  ( module Databrary.Model.Party.Types
  , nobodyParty
  , rootParty
  , staffParty
  , partyName
  , partyEmail
  , lookupParty
  , lookupPartyAuthorizations
  , lookupAuthParty
  , lookupSiteAuthByEmail
  , changeParty
  , changeAccount
  , addParty
  , addAccount
  , removeParty
  , auditAccountLogin
  , recentAccountLogins
  , partyRowJSON
  , partyJSON
  , PartyFilter(..)
  , findParties
  , lookupAvatar
  , changeAvatar
  ) where

import Control.Applicative ((<|>))
import Control.Exception.Lifted (handleJust)
import Control.Monad (guard)
import qualified Data.ByteString as BS
import Data.Int (Int64)
import Data.List (intercalate)
import Data.Maybe (isNothing, fromMaybe)
import Data.Monoid ((<>))
import qualified Data.Text as T
import Database.PostgreSQL.Typed (pgSQL)
import Database.PostgreSQL.Typed.Query (unsafeModifyQuery)
import Database.PostgreSQL.Typed.Dynamic (pgLiteralRep, pgLiteralString, pgSafeLiteral)

import Databrary.Ops
import Databrary.Has (Has(..), peek)
import Databrary.Service.DB
import qualified Databrary.JSON as JSON
import Databrary.HTTP.Request
import Databrary.Model.Id
import Databrary.Model.SQL
import Databrary.Model.Paginate
import Databrary.Model.Permission
import Databrary.Model.Audit
import Databrary.Model.Audit.SQL
import Databrary.Model.Identity.Types
import Databrary.Model.Volume
import Databrary.Model.Asset.Types
import Databrary.Model.Asset.SQL
import Databrary.Model.Party.Types
import Databrary.Model.Party.SQL
import Databrary.Model.Party.Boot

nobodyParty, rootParty, staffParty :: Party
nobodyParty = $(loadParty (Id (-1)) PermissionREAD)
rootParty = $(loadParty (Id 0) PermissionSHARED)
staffParty = $(loadParty (Id 2) PermissionPUBLIC)

partyName :: PartyRow -> T.Text
partyName PartyRow{ partyPreName = Just p, partySortName = n } = p <> T.cons ' ' n
partyName PartyRow{ partySortName = n } = n

emailPermission :: Permission
emailPermission = PermissionSHARED

showEmail :: Identity -> Bool
showEmail i = accessSite i >= emailPermission

partyEmail :: Party -> Maybe BS.ByteString
partyEmail p =
  guard (partyPermission p >= emailPermission) >> accountEmail <$> partyAccount p

partyRowJSON :: JSON.ToObject o => PartyRow -> JSON.Record (Id Party) o
partyRowJSON PartyRow{..} = JSON.Record partyId $
     "sortname" JSON..= partySortName
  <> "prename" JSON..=? partyPreName
  <> "orcid" JSON..=? (show <$> partyORCID)
  <> "affiliation" JSON..=? partyAffiliation
  <> "url" JSON..=? partyURL

partyJSON :: JSON.ToObject o => Party -> JSON.Record (Id Party) o
partyJSON p@Party{..} = partyRowJSON partyRow JSON..<>
     "institution" JSON..=? (True <? isNothing partyAccount)
  <> "email" JSON..=? partyEmail p
  <> "permission" JSON..=? (partyPermission <? partyPermission > PermissionREAD)

changeParty :: MonadAudit c m => Party -> m ()
changeParty p = do
  ident <- getAuditIdentity
  dbExecute1' $(updateParty 'ident 'p)

changeAccount :: MonadAudit c m => SiteAuth -> m ()
changeAccount a = do
  ident <- getAuditIdentity
  dbExecute1' $(updateAccount 'ident 'a)

addParty :: MonadAudit c m => Party -> m Party
addParty bp = do
  ident <- getAuditIdentity
  dbQuery1' $ fmap (\p -> Party p Nothing PermissionREAD Nothing) $(insertParty 'ident 'bp)

addAccount :: MonadAudit c m => Account -> m Account
addAccount ba@Account{ accountParty = bp } = do
  ident <- getAuditIdentity
  p <- dbQuery1' $ fmap (\p -> Party p Nothing PermissionREAD Nothing) $(insertParty 'ident 'bp)
  let pa = p{ partyAccount = Just a }
      a = ba{ accountParty = pa }
  dbExecute1' $(insertAccount 'ident 'a)
  return a

removeParty :: MonadAudit c m => Party -> m Bool
removeParty p = do
  ident <- getAuditIdentity
  dbTransaction $ handleJust (guard . isForeignKeyViolation) (\_ -> return False) $ do
    _ <- dbExecute1 $(deleteAccount 'ident 'p)
    dbExecute1 $(deleteParty 'ident 'p)

lookupFixedParty :: Id Party -> Identity -> Maybe Party
lookupFixedParty (Id (-1)) _ = Just nobodyParty
lookupFixedParty (Id 0) i = Just rootParty{ partyPermission = accessPermission i `max` PermissionSHARED, partyAccess = accessMember i > PermissionNONE ?> view i }
lookupFixedParty i a = view a <? (i == view a)

lookupParty :: (MonadDB c m, MonadHasIdentity c m) => Id Party -> m (Maybe Party)
lookupParty i = do
  ident <- peek
  lookupFixedParty i ident `orElseM`
    dbQuery1 $(selectQuery (selectParty 'ident) "$WHERE party.id = ${i}")

lookupPartyAuthorizations :: (MonadDB c m, MonadHasIdentity c m) => m [(Party, Maybe Permission)]
lookupPartyAuthorizations = do
  ident <- peek
  dbQuery $(selectQuery (selectPartyAuthorization 'ident) "WHERE party.id > 0")

lookupAuthParty :: (MonadDB c m, MonadHasIdentity c m) => Id Party -> m (Maybe Party)
lookupAuthParty i = do
  ident <- peek
  lookupFixedParty i ident `orElseM`
    dbQuery1 $(selectQuery (selectAuthParty 'ident) "$WHERE party.id = ${i}")

lookupSiteAuthByEmail :: MonadDB c m => Bool -> BS.ByteString -> m (Maybe SiteAuth)
lookupSiteAuthByEmail ic e = do
  r <- dbQuery1 $(selectQuery selectSiteAuth "WHERE account.email = ${e}")
  if ic && isNothing r
    then do
      a <- dbQuery $(selectQuery selectSiteAuth "WHERE lower(account.email) = lower(${e}) LIMIT 2")
      return $ case a of
        [x] -> Just x
        _ -> Nothing
    else
      return r

auditAccountLogin :: (MonadHasRequest c m, MonadDB c m) => Bool -> Party -> BS.ByteString -> m ()
auditAccountLogin success who email = do
  ip <- getRemoteIp
  dbExecute1' [pgSQL|INSERT INTO audit.account (audit_action, audit_user, audit_ip, id, email) VALUES
    (${if success then AuditActionOpen else AuditActionAttempt}, -1, ${ip}, ${partyId $ partyRow who}, ${email})|]

recentAccountLogins :: MonadDB c m => Party -> m Int64
recentAccountLogins who = fromMaybe 0 <$>
  dbQuery1 [pgSQL|!SELECT count(*) FROM audit.account WHERE audit_action = 'attempt' AND id = ${partyId $ partyRow who} AND audit_time > CURRENT_TIMESTAMP - interval '1 hour'|]

data PartyFilter = PartyFilter
  { partyFilterQuery :: Maybe String
  , partyFilterAuthorization :: Maybe Permission
  , partyFilterInstitution :: Maybe Bool
  , partyFilterPaginate :: Paginate
  }

instance Monoid PartyFilter where
  mempty = PartyFilter Nothing Nothing Nothing def
  mappend (PartyFilter q1 a1 i1 p) (PartyFilter q2 a2 i2 _) =
    PartyFilter (q1 <> q2) (a1 <|> a2) (i1 <|> i2) p

partyFilter :: PartyFilter -> Identity -> BS.ByteString
partyFilter PartyFilter{..} ident = BS.concat
  [ withq partyFilterAuthorization (const " JOIN authorize_view ON party.id = child AND parent = 0")
  , " WHERE id > 0 AND id != ", pgLiteralRep (partyId $ partyRow $ staffParty)
  , withq partyFilterQuery (\n -> " AND " <> queryVal <> " ILIKE " <> pgLiteralRep (wordPat n))
  , withq partyFilterAuthorization (\a -> " AND site = " <> pgSafeLiteral a)
  , withq partyFilterInstitution (\i -> if i then " AND account.id IS NULL" else " AND account.password IS NOT NULL")
  , " ORDER BY name, prename "
  , paginateSQL partyFilterPaginate
  ]
  where
  withq v f = maybe "" f v
  wordPat = intercalate "%" . ("":) . (++[""]) . words
  queryVal
    | showEmail ident = "(COALESCE(prename || ' ', '') || name || COALESCE(' ' || email, ''))"
    | otherwise = "(COALESCE(prename || ' ', '') || name)"

findParties :: (MonadHasIdentity c m, MonadDB c m) => PartyFilter -> m [Party]
findParties pf = do
  ident <- peek
  dbQuery $ unsafeModifyQuery $(selectQuery (selectParty 'ident) "")
    (<> partyFilter pf ident)

lookupAvatar :: MonadDB c m => Id Party -> m (Maybe Asset)
lookupAvatar p =
  dbQuery1 $ (`Asset` coreVolume) <$> $(selectQuery selectAssetRow $ "$JOIN avatar ON asset.id = avatar.asset WHERE avatar.party = ${p} AND asset.volume = " ++ pgLiteralString (volumeId $ volumeRow coreVolume))

changeAvatar :: MonadAudit c m => Party -> Maybe Asset -> m Bool
changeAvatar p Nothing = do
  ident <- getAuditIdentity
  dbExecute1 $(auditDelete 'ident "avatar" "party = ${partyId $ partyRow p}" Nothing)
changeAvatar p (Just a) = do
  ident <- getAuditIdentity
  (0 <) . fst <$> updateOrInsert
    $(auditUpdate 'ident "avatar" [("asset", "${assetId $ assetRow a}")] "party = ${partyId $ partyRow p}" Nothing)
    $(auditInsert 'ident "avatar" [("asset", "${assetId $ assetRow a}"), ("party", "${partyId $ partyRow p}")] Nothing)
