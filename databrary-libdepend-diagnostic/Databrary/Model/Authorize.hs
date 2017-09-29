{-# LANGUAGE TemplateHaskell, RecordWildCards, OverloadedStrings, ScopedTypeVariables, DataKinds #-}
module Databrary.Model.Authorize
  ( module Databrary.Model.Authorize.Types
  , selfAuthorize
  , lookupAuthorizedChildren
  , lookupAuthorizedParents
  , lookupAuthorize
  , lookupAuthorizeParent
  , lookupAuthorization
  , changeAuthorize
  , removeAuthorize
  , authorizeExpired
  , authorizeActive
  , authorizeJSON
  , lookupAuthorizeActivity
  ) where

import Control.Monad (when)
import Data.Int (Int64)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Database.PostgreSQL.Typed.Query (makePGQuery, QueryFlags(..), simpleQueryFlags)

import Databrary.Has (peek, view)
import qualified Databrary.JSON as JSON
import Databrary.Service.DB
import Databrary.Model.SQL
import Databrary.Model.Time
import Databrary.Model.Id
import Databrary.Model.Audit
import Databrary.Model.Permission
import Databrary.Model.Party
import Databrary.Model.Identity.Types
import Databrary.Model.Authorize.Types
import Databrary.Model.Authorize.SQL

$(useTDB)

selfAuthorize :: Party -> Authorize
selfAuthorize p =
  Authorize (Authorization (if partyId (partyRow p) == partyId (partyRow nobodyParty) then minBound else maxBound) p p) Nothing

lookupAuthorizedParents :: (MonadDB c m, MonadHasIdentity c m) => Party -> Maybe Permission -> m [Authorize]
lookupAuthorizedParents child perm = do
  ident <- peek
  dbQuery $ maybe
    $(selectQuery (selectAuthorizeParent 'child 'ident) "$")
    (\p -> $(selectQuery (selectAuthorizeParent 'child 'ident) "$WHERE (expires IS NULL OR expires > CURRENT_TIMESTAMP) AND site >= ${p} AND member >= ${p} AND (site <> 'NONE' OR member <> 'NONE')"))
    perm

lookupAuthorizedChildren :: (MonadDB c m, MonadHasIdentity c m) => Party -> Maybe Permission -> m [Authorize]
lookupAuthorizedChildren parent perm = do
  ident <- peek
  dbQuery $ maybe
    $(selectQuery (selectAuthorizeChild 'parent 'ident) "$")
    (\p -> $(selectQuery (selectAuthorizeChild 'parent 'ident) "$WHERE (expires IS NULL OR expires > CURRENT_TIMESTAMP) AND site >= ${p} AND member >= ${p} AND (site <> 'NONE' OR member <> 'NONE')"))
    perm

lookupAuthorize :: (MonadDB c m, MonadHasIdentity c m) => Party -> Party -> m (Maybe Authorize)
lookupAuthorize child parent = do
  mRow <- dbQuery1
      $(makePGQuery
          (simpleQueryFlags { flagPrepare = Just [] })
          (   "SELECT authorize.site,authorize.member,authorize.expires"
           ++ " FROM authorize " 
           ++ "WHERE authorize.child = ${partyId $ partyRow child} AND authorize.parent = ${partyId $ partyRow parent} AND (expires IS NULL OR expires > CURRENT_TIMESTAMP)"))
  pure (fmap (\(si,me,ex) -> makeAuthorize (Access si me) ex child parent) mRow)

lookupAuthorizeParent :: (MonadDB c m, MonadHasIdentity c m) => Party -> Id Party -> m (Maybe Authorize)
lookupAuthorizeParent child parent = do
  ident <- peek
  dbQuery1 $ $(selectQuery (selectAuthorizeParent 'child 'ident) "$WHERE authorize.parent = ${parent} AND (expires IS NULL OR expires > CURRENT_TIMESTAMP)")

lookupAuthorization :: (MonadDB c m, MonadHasIdentity c m) => Party -> Party -> m Authorization
lookupAuthorization child parent
  | partyId (partyRow child) == partyId (partyRow parent) = return $ authorization $ selfAuthorize child
  | otherwise = do
    auth <- peek
    if partyId (view auth) == partyId (partyRow child) && partyId (partyRow parent) == partyId (partyRow rootParty)
      then return $ Authorization (siteAccess auth) child parent
      else fromMaybe (Authorization mempty child parent) <$>
        dbQuery1 ((\a -> a child parent) <$> $(selectQuery authorizationRow "!$WHERE authorize_view.child = ${partyId $ partyRow child} AND authorize_view.parent = ${partyId $ partyRow parent}"))

changeAuthorize :: (MonadAudit c m) => Authorize -> m ()
changeAuthorize auth = do
  ident <- getAuditIdentity
  (r, _) <- updateOrInsert
    $(updateAuthorize 'ident 'auth)
    $(insertAuthorize 'ident 'auth)
  when (r /= 1) $ fail $ "changeAuthorize: " ++ show r ++ " rows"

removeAuthorize :: (MonadAudit c m) => Authorize -> m Bool
removeAuthorize auth = do
  ident <- getAuditIdentity
  dbExecute1 $(deleteAuthorize 'ident 'auth)

authorizationActive :: Authorization -> Bool
authorizationActive Authorization{ authorizeAccess = a } = a /= mempty

authorizeExpired :: Authorize -> Timestamp -> Bool
authorizeExpired Authorize{ authorizeExpires = Just e } = (e <)
authorizeExpired _ = const False

authorizeActive :: Authorize -> Timestamp -> Bool
authorizeActive a t = authorizationActive (authorization a) && not (authorizeExpired a t)

authorizeJSON :: JSON.ToObject o => Authorize -> o
authorizeJSON Authorize{..} = accessJSON (authorizeAccess authorization)
  <> "expires" JSON..=? authorizeExpires

lookupAuthorizeActivity :: (MonadDB c m, MonadHasIdentity c m) => Int -> m [(Timestamp, Party)]
lookupAuthorizeActivity limit = do
  ident :: Identity <- peek
  dbQuery $(selectQuery (selectAuthorizeActivity 'ident) "$JOIN authorize_view ON audit.parent = authorize_view.child AND authorize_view.parent = 0 WHERE audit.audit_action IN ('add','change') AND audit.site >= 'EDIT' AND authorize_view.site > 'EDIT' ORDER BY audit.audit_time DESC LIMIT ${fromIntegral limit :: Int64}")
