module Databrary.Controller.VolumeAccess where

import Databrary.Model.Id.Types
import Databrary.Model.Volume.Types
import Databrary.Action
import Databrary.Controller.Paths

viewVolumeAccess :: ActionRoute (Id Volume, VolumeAccessTarget)
postVolumeAccess :: ActionRoute (API, (Id Volume, VolumeAccessTarget))
