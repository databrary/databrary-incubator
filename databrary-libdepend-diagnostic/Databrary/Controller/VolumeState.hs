{-# LANGUAGE OverloadedStrings #-}
module Databrary.Controller.VolumeState
  ( postVolumeState
  , deleteVolumeState
  ) where

import Data.Maybe (fromMaybe)
import Network.HTTP.Types (noContent204)
import qualified Web.Route.Invertible as R

import qualified Databrary.JSON as JSON
import Databrary.Model.Id
import Databrary.Model.Permission
import Databrary.Model.Volume
import Databrary.Model.VolumeState
import Databrary.HTTP.Path.Parser
import Databrary.HTTP.Form.Deform
import Databrary.Action.Route
import Databrary.Action
import Databrary.Controller.Form
import Databrary.Controller.Paths
import Databrary.Controller.Volume

postVolumeState :: ActionRoute (Id Volume, VolumeStateKey)
postVolumeState = action PUT (pathJSON >/> pathId </> "state" >/> R.parameter) $ \(vi, k) -> withAuth $ do
  v <- getVolume PermissionEDIT vi
  s <- runForm Nothing $ do
    j <- deform
    p <- "public" .:> fromMaybe False <$> deformOptional deform
    return VolumeState
      { stateVolume = v
      , volumeStateKey = k
      , volumeStatePublic = p
      , volumeStateValue = j
      }
  changeVolumeState s
  return $ emptyResponse noContent204 []

deleteVolumeState :: ActionRoute (Id Volume, VolumeStateKey)
deleteVolumeState = action DELETE (pathJSON >/> pathId </> "state" >/> R.parameter) $ \(vi, k) -> withAuth $ do
  v <- getVolume PermissionEDIT vi
  r <- removeVolumeState v k
  return $ okResponse [] $ JSON.toEncoding r
