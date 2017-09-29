{-# LANGUAGE DataKinds, TemplateHaskell, DeriveDataTypeable, OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Databrary.Model.Audit.Types
  ( AuditAction(..)
  , AuditIdentity(..)
  , Audit(..)
  ) where

import Database.PostgreSQL.Typed.Inet (PGInet)

import Databrary.Model.Time
import Databrary.Model.Enum
import Databrary.Model.Id.Types
import Databrary.Model.Party.Types

makeDBEnum "audit.action" "AuditAction"

data AuditIdentity = AuditIdentity
  { auditWho :: !(Id Party)
  , auditIp :: !PGInet
  } deriving (Eq)

data Audit = Audit
  { auditWhen :: !Timestamp
  , auditIdentity :: !AuditIdentity
  , auditAction :: !AuditAction
  }
