{-# LANGUAGE OverloadedStrings #-}
module Databrary.View.Record
  ( htmlRecordForm
  , htmlRecordMeasureForm
  ) where

import qualified Data.ByteString.Char8 as BSC

import Databrary.Model.Volume
import Databrary.Model.Record
import Databrary.Model.Category
import Databrary.Model.Metric
import Databrary.Action
import Databrary.View.Form

import {-# SOURCE #-} Databrary.Controller.Record

htmlRecordForm :: Volume -> RequestContext -> FormHtml f
htmlRecordForm vol = htmlForm "Create record"
  createRecord (HTML, volumeId $ volumeRow vol)
  (field "category" $ inputSelect Nothing $ ("", "<record>") : map (\c -> (BSC.pack $ show $ categoryId c, categoryName c)) allCategories)
  (const mempty)

htmlRecordMeasureForm :: Record -> Metric -> RequestContext -> FormHtml f
htmlRecordMeasureForm rec met = htmlForm "Set measure"
  postRecordMeasure (HTML, recordId $ recordRow rec, metricId met)
  (field "datum" $ inputText (Nothing :: Maybe String))
  (const mempty)
