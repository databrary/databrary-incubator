module Databrary.Controller.Comment where

import Databrary.Model.Id.Types
import Databrary.Model.Slot.Types
import Databrary.Action

postComment :: ActionRoute (API, Id Slot)
