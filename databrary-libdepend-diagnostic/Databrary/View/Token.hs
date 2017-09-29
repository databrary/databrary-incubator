{-# LANGUAGE OverloadedStrings #-}
module Databrary.View.Token
  ( htmlPasswordToken
  ) where

import Databrary.Model.Id
import Databrary.Model.Token
import Databrary.Action
import Databrary.View.Form

import {-# SOURCE #-} Databrary.Controller.Token

htmlPasswordToken :: Id LoginToken -> RequestContext -> FormHtml f
htmlPasswordToken tok = htmlForm "Reset Password"
  postPasswordToken (HTML, tok)
  (do
    field "once" inputPassword
    field "again" inputPassword)
  (const mempty)
