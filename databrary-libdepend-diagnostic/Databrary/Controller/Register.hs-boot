module Databrary.Controller.Register where

import Databrary.Model.Id.Types
import Databrary.Model.Party.Types
import Databrary.Action

viewRegister :: ActionRoute ()
postRegister :: ActionRoute API
viewPasswordReset :: ActionRoute ()
postPasswordReset :: ActionRoute API
resendInvestigator :: ActionRoute (Id Party)
