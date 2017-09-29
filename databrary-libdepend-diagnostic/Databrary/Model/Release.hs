{-# LANGUAGE TemplateHaskell, DataKinds #-}
module Databrary.Model.Release
  ( module Databrary.Model.Release.Types
  , changeRelease
  ) where

import Control.Monad (guard)

import Databrary.Service.DB
import Databrary.Model.SQL
import Databrary.Model.Audit
import Databrary.Model.Slot.Types
import Databrary.Model.Container.Types
import Databrary.Model.Release.Types
import Databrary.Model.Release.SQL

useTDB

changeRelease :: MonadAudit c m => Slot -> Maybe Release -> m Bool
changeRelease s Nothing = do
  ident <- getAuditIdentity
  dbExecute1 $(deleteRelease 'ident 's)
changeRelease s (Just c) = do
  ident <- getAuditIdentity
  either (const False) ((0 <) . fst) <$> tryUpdateOrInsert (guard . isExclusionViolation)
    $(updateRelease 'ident 's 'c)
    $(insertRelease 'ident 's 'c)
