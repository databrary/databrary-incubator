module Databrary.Controller.Slot where

import Databrary.Model.Id.Types
import Databrary.Model.Volume.Types
import Databrary.Model.Slot.Types
import Databrary.Action

viewSlot :: ActionRoute (API, (Maybe (Id Volume), Id Slot))
