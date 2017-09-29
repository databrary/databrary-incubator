{-# LANGUAGE TemplateHaskell, QuasiQuotes, RecordWildCards, DataKinds #-}
module Databrary.Model.Audit
  ( module Databrary.Model.Audit.Types
  , MonadAudit
  , getRemoteIp
  , getAuditIdentity
  , Analytic(..)
  , auditAnalytic
  ) where

import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Database.PostgreSQL.Typed (pgSQL)
import Database.PostgreSQL.Typed.Inet (PGInet(..), sockAddrPGInet)
import Network.Wai (remoteHost)

import Databrary.Has
import qualified Databrary.JSON as JSON
import Databrary.Service.DB
import Databrary.HTTP.Request
import Databrary.Model.Id.Types
import Databrary.Model.Party.Types
import Databrary.Model.Audit.Types

useTDB

type MonadAudit c m = (MonadHasRequest c m, MonadHas (Id Party) c m, MonadDB c m)

getRemoteIp :: MonadHasRequest c m => m PGInet
getRemoteIp = peeks (fromMaybe (PGInet 0 32) . sockAddrPGInet . remoteHost)

getAuditIdentity :: (MonadHasRequest c m, MonadHas (Id Party) c m) => m AuditIdentity
getAuditIdentity = AuditIdentity <$> peek <*> getRemoteIp

data Analytic = Analytic
  { analyticAction :: AuditAction
  , analyticRoute :: T.Text
  , analyticData :: Maybe JSON.Value
  }

auditAnalytic :: (MonadAudit c m) => Analytic -> m ()
auditAnalytic Analytic{..} = do
  ai <- getAuditIdentity
  dbExecute1' [pgSQL|INSERT INTO audit.analytic (audit_action, audit_user, audit_ip, route, data) VALUES
    (${analyticAction}, ${auditWho ai}, ${auditIp ai}, ${analyticRoute}, ${analyticData})|]
