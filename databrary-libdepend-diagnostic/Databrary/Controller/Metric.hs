module Databrary.Controller.Metric
  ( postVolumeMetric
  , deleteVolumeMetric
  ) where

import Control.Invertible.Monoidal ((>|<))

import qualified Databrary.JSON as JSON
import Databrary.Model.Id
import Databrary.Model.Permission
import Databrary.Model.Volume
import Databrary.Model.Category
import Databrary.Model.Metric
import Databrary.Model.VolumeMetric
import Databrary.HTTP.Path.Parser
import Databrary.Action.Route
import Databrary.Action
import Databrary.Controller.Paths
import Databrary.Controller.Volume

postVolumeMetric :: ActionRoute (Id Volume, Either (Id Category) (Id Metric))
postVolumeMetric = action PUT (pathJSON >/> pathId </> (pathId >|< pathId)) $ \(vi, cm) -> withAuth $ do
  v <- getVolume PermissionEDIT vi
  r <- either (addVolumeCategory v) (\m -> do
    r <- addVolumeMetric v m
    return $ if r then [m] else []) cm
  return $ okResponse [] $ JSON.toEncoding r

deleteVolumeMetric :: ActionRoute (Id Volume, Either (Id Category) (Id Metric))
deleteVolumeMetric = action DELETE (pathJSON >/> pathId </> (pathId >|< pathId)) $ \(vi, cm) -> withAuth $ do
  v <- getVolume PermissionEDIT vi
  r <- either (removeVolumeCategory v) (fmap fromEnum . removeVolumeMetric v) cm
  return $ okResponse [] $ JSON.toEncoding r
