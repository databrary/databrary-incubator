{-# LANGUAGE OverloadedStrings #-}
module Databrary.Controller.Email
  ( testEmail

  ) where

import Databrary.Action
import Databrary.View.Email
import Databrary.HTTP.Path.Parser

testEmail :: ActionRoute ()
testEmail = action GET ("email" </< "test") $ \() -> do
  okResponse [] emailNewlyAuthorized