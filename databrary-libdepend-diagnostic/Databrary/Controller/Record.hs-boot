module Databrary.Controller.Record where

import Databrary.Model.Id.Types
import Databrary.Model.Volume.Types
import Databrary.Model.Record.Types
import Databrary.Model.Metric.Types
import Databrary.Action

createRecord :: ActionRoute (API, Id Volume)
postRecordMeasure :: ActionRoute (API, Id Record, Id Metric)
