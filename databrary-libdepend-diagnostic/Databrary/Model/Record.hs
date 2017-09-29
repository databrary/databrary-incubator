{-# LANGUAGE OverloadedStrings, TemplateHaskell, RecordWildCards, DataKinds #-}
module Databrary.Model.Record
  ( module Databrary.Model.Record.Types
  , blankRecord
  , lookupRecord
  , lookupVolumeRecord
  , lookupVolumeRecords
  , addRecord
  , changeRecord
  , removeRecord
  , recordJSON
  ) where

import Control.Monad (guard)
import Data.Either (isRight)
import Data.Monoid ((<>))

import Databrary.Has (peek, view)
import Databrary.Service.DB
import qualified Databrary.JSON as JSON
import Databrary.Model.SQL
import Databrary.Model.Audit
import Databrary.Model.Id
import Databrary.Model.Identity.Types
import Databrary.Model.Volume.Types
import Databrary.Model.Party.Types
import Databrary.Model.Category
import Databrary.Model.Measure
import Databrary.Model.Record.Types
import Databrary.Model.Record.SQL

blankRecord :: Category -> Volume -> Record
blankRecord cat vol = Record
  { recordRow = RecordRow
    { recordId = error "blankRecord"
    , recordCategory = cat
    }
  , recordVolume = vol
  , recordRelease = Nothing
  , recordMeasures = []
  }

lookupRecord :: (MonadHasIdentity c m, MonadDB c m) => Id Record -> m (Maybe Record)
lookupRecord ri = do
  ident <- peek
  dbQuery1 $(selectQuery (selectRecord 'ident) "$WHERE record.id = ${ri}")

lookupVolumeRecord :: MonadDB c m => Volume -> Id Record -> m (Maybe Record)
lookupVolumeRecord vol ri =
  dbQuery1 $ fmap ($ vol) $(selectQuery selectVolumeRecord "$WHERE record.id = ${ri} AND record.volume = ${volumeId $ volumeRow vol}")

lookupVolumeRecords :: MonadDB c m => Volume -> m [Record]
lookupVolumeRecords vol =
  dbQuery $ fmap ($ vol) $(selectQuery selectVolumeRecord "$WHERE record.volume = ${volumeId $ volumeRow vol}")

addRecord :: MonadAudit c m => Record -> m Record
addRecord br = do
  ident <- getAuditIdentity
  dbQuery1' $(insertRecord 'ident 'br)

changeRecord :: MonadAudit c m => Record -> m ()
changeRecord r = do
  ident <- getAuditIdentity
  dbExecute1' $(updateRecord 'ident 'r)

removeRecord :: MonadAudit c m => Record -> m Bool
removeRecord r = do
  ident <- getAuditIdentity
  isRight <$> dbTryJust (guard . isForeignKeyViolation) (dbExecute1 $(deleteRecord 'ident 'r))

recordJSON :: JSON.ToNestedObject o u => Record -> JSON.Record (Id Record) o
recordJSON r@Record{ recordRow = RecordRow{..}, ..} = JSON.Record recordId $
  -- "volume" JSON..= volumeId recordVolume
     "category" JSON..= categoryId recordCategory
  <> "measures" JSON..=. measuresJSON (getRecordMeasures r)
