{-# LANGUAGE OverloadedStrings, TemplateHaskell, DataKinds #-}
module Databrary.Model.Measure
  ( getRecordMeasures
  , getMeasure
  , changeRecordMeasure
  , removeRecordMeasure
  , decodeMeasure
  , measuresJSON
  ) where

import Control.Monad (guard)
import Data.List (find)
import Data.Maybe (fromMaybe)
import Data.Ord (comparing)
import qualified Data.Text as T
import Database.PostgreSQL.Typed.Protocol (PGError(..), pgErrorCode)
import Database.PostgreSQL.Typed.Types (PGTypeName, pgTypeName, PGColumn(pgDecode))

import Databrary.Ops
import Databrary.Has (view)
import Databrary.Service.DB
import qualified Databrary.JSON as JSON
import Databrary.Model.SQL
import Databrary.Model.Permission
import Databrary.Model.Audit
import Databrary.Model.Metric
import Databrary.Model.Record.Types
import Databrary.Model.Measure.SQL

measureOrder :: Measure -> Measure -> Ordering
measureOrder = comparing $ metricId . measureMetric

getMeasure :: Metric -> Measures -> Maybe Measure
getMeasure m = find ((metricId m ==) . metricId . measureMetric)

rmMeasure :: Measure -> Record
rmMeasure m@Measure{ measureRecord = rec } = rec{ recordMeasures = upd $ recordMeasures rec } where
  upd [] = [m]
  upd l@(m':l') = case m `measureOrder` m' of
    GT -> m':upd l'
    EQ -> l'
    LT -> l

upMeasure :: Measure -> Record
upMeasure m@Measure{ measureRecord = rec } = rec{ recordMeasures = upd $ recordMeasures rec } where
  upd [] = [m]
  upd l@(m':l') = case m `measureOrder` m' of
    GT -> m':upd l'
    EQ -> m:l'
    LT -> m:l

isInvalidInputException :: PGError -> Bool
isInvalidInputException e = pgErrorCode e `elem` ["22007", "22008", "22P02"]

changeRecordMeasure :: MonadAudit c m => Measure -> m (Maybe Record)
changeRecordMeasure m = do
  ident <- getAuditIdentity
  r <- tryUpdateOrInsert (guard . isInvalidInputException)
    $(updateMeasure 'ident 'm)
    $(insertMeasure 'ident 'm)
  case r of
    Left () -> return Nothing
    Right (_, [d]) -> return $ Just $ upMeasure d
    Right (n, _) -> fail $ "changeRecordMeasure: " ++ show n ++ " rows"

removeRecordMeasure :: MonadAudit c m => Measure -> m Record
removeRecordMeasure m = do
  ident <- getAuditIdentity
  r <- dbExecute1 $(deleteMeasure 'ident 'm)
  return $ if r
    then rmMeasure m
    else measureRecord m

getRecordMeasures :: Record -> Measures
getRecordMeasures r = maybe [] filt $ readRelease (view r) where
  filt rr = filter ((rr <=) . fromMaybe (view r) . view) $ recordMeasures r

decodeMeasure :: PGColumn t d => PGTypeName t -> Measure -> Maybe d
decodeMeasure t Measure{ measureMetric = Metric{ metricType = m }, measureDatum = d } =
  pgTypeName t == show m ?> pgDecode t d

measureJSONPair :: JSON.KeyValue kv => Measure -> kv
measureJSONPair m = T.pack (show (metricId (measureMetric m))) JSON..= measureDatum m

measuresJSON :: JSON.ToObject o => Measures -> o
measuresJSON = foldMap measureJSONPair
