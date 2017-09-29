module Databrary.Controller.Container where

import Databrary.Model.Id.Types
import Databrary.Model.Volume.Types
import Databrary.Model.Container.Types
import Databrary.Model.Slot.Types
import Databrary.Action

viewContainer :: ActionRoute (API, (Maybe (Id Volume), Id Container))
postContainer :: ActionRoute (API, Id Slot)
createContainer :: ActionRoute (API, Id Volume)
