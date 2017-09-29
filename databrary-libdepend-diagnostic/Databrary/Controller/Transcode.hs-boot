module Databrary.Controller.Transcode where

import Databrary.Model.Id.Types
import Databrary.Model.Transcode.Types
import Databrary.Action

data TranscodeAction
  = TranscodeStart
  | TranscodeStop
  | TranscodeFail
instance Show TranscodeAction

remoteTranscode :: ActionRoute (Id Transcode)
postTranscode :: ActionRoute (Id Transcode)
