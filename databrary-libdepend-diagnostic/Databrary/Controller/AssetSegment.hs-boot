module Databrary.Controller.AssetSegment where

import Databrary.Model.Id.Types
import Databrary.Model.Volume.Types
import Databrary.Model.Slot.Types
import Databrary.Model.Asset.Types
import Databrary.Model.AssetSegment.Types
import Databrary.Action

viewAssetSegment :: ActionRoute (API, Maybe (Id Volume), Id Slot, Id Asset)
serveAssetSegment :: Bool -> AssetSegment -> ActionM Response
downloadAssetSegment :: ActionRoute (Id Slot, Id Asset)
