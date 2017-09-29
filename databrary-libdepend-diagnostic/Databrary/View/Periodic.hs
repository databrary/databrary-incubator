{-# LANGUAGE OverloadedStrings #-}
module Databrary.View.Periodic
  ( htmlPeriodic
  ) where

import Databrary.Action.Types
import Databrary.View.Form

import {-# SOURCE #-} Databrary.Controller.Periodic

htmlPeriodic :: RequestContext -> FormHtml f
htmlPeriodic = htmlForm
  "run periodic"
  postPeriodic ()
  (field "weekly" $ inputCheckbox False)
  (const mempty)
