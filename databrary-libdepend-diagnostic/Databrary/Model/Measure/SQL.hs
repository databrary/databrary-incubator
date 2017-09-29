{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Databrary.Model.Measure.SQL
  ( insertMeasure
  , updateMeasure
  , deleteMeasure
  ) where

import qualified Language.Haskell.TH as TH

import Databrary.Model.SQL.Select
import Databrary.Model.Audit.SQL
import Databrary.Model.Metric.Types
import Databrary.Model.Record.Types

setMeasureDatum :: Measure -> MeasureDatum -> Measure
setMeasureDatum m d = m{ measureDatum = d }

measureDatumRow :: Selector -- ^ @'MeasureDatum'@
measureDatumRow = selectColumn "measure" "datum"

measureKeys :: String -- ^ @'Measure'@
  -> [(String, String)]
measureKeys m =
  [ ("record", "${recordId $ recordRow $ measureRecord " ++ m ++ "}")
  , ("metric", "${metricId $ measureMetric " ++ m ++ "}")
  ]

measureSets :: String -- ^ @'Record'@
  -> [(String, String)]
measureSets r =
  [ ("datum", "${measureDatum " ++ r ++ "}")
  ]

insertMeasure :: TH.Name -- ^ @'AuditIdentity'@
  -> TH.Name -- ^ @'Measure'@
  -> TH.ExpQ -- ^ @'Measure'@
insertMeasure ident m = auditInsert ident "!measure"
  (measureKeys (nameRef m) ++ measureSets (nameRef m))
  (Just $ selectOutput $ selectMap ((TH.VarE 'setMeasureDatum `TH.AppE` TH.VarE m) `TH.AppE`) measureDatumRow)

updateMeasure :: TH.Name -- ^ @'AuditIdentity'@
  -> TH.Name -- ^ @'Measure'@
  -> TH.ExpQ -- ^ @'Measure'@
updateMeasure ident m = auditUpdate ident "!measure"
  (measureSets (nameRef m))
  (whereEq $ measureKeys (nameRef m))
  (Just $ selectOutput $ selectMap ((TH.VarE 'setMeasureDatum `TH.AppE` TH.VarE m) `TH.AppE`) measureDatumRow)

deleteMeasure :: TH.Name -- ^ @'AuditIdentity'@
  -> TH.Name -- ^ @'Measure'@
  -> TH.ExpQ -- ^ @()@
deleteMeasure ident m = auditDelete ident "measure"
  (whereEq $ measureKeys (nameRef m))
  Nothing

