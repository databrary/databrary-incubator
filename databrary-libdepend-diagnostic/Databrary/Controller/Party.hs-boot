module Databrary.Controller.Party where

import Databrary.Action.Route
import Databrary.Model.Id.Types
import Databrary.Model.Party.Types
import Databrary.Controller.Paths

viewParty :: ActionRoute (API, PartyTarget)
viewPartyEdit :: ActionRoute PartyTarget
postParty :: ActionRoute (API, PartyTarget)
createParty :: ActionRoute API
deleteParty :: ActionRoute (Id Party)
viewPartyDelete :: ActionRoute (Id Party)
viewAvatar :: ActionRoute (Id Party)
queryParties :: ActionRoute API
adminParties :: ActionRoute ()
