module Databrary.Controller.Token where

import Databrary.Model.Id.Types
import Databrary.Model.Token.Types
import Databrary.Action

viewLoginToken :: ActionRoute (API, Id LoginToken)
postPasswordToken :: ActionRoute (API, Id LoginToken)
