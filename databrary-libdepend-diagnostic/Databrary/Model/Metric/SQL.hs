{-# LANGUAGE TemplateHaskell #-}
module Databrary.Model.Metric.SQL
  ( makeMetric
  ) where

import Data.Maybe (fromMaybe)
import qualified Data.Text as T

import Databrary.Model.Id.Types
import Databrary.Model.Release.Types
import Databrary.Model.Metric.Types
import Databrary.Model.Category
import Databrary.Model.SQL.Select

makeMetric :: Id Metric -> Id Category -> T.Text -> Maybe Release -> MeasureType -> Maybe [Maybe MeasureDatum] -> Maybe MeasureDatum -> Maybe T.Text -> Maybe Bool -> Metric
makeMetric i c n r t o = Metric i (getCategory' c) n r t (maybe [] (map (fromMaybe (error "NULL measure.option"))) o)

