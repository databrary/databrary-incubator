{-# LANGUAGE OverloadedStrings #-}
module Databrary.Controller.Periodic
  ( viewPeriodic
  , postPeriodic
  ) where

import Control.Exception (throwTo)
import Control.Monad.IO.Class (liftIO)

import Databrary.Has
import Databrary.Model.Periodic
import Databrary.Service.Types
import Databrary.Action
import Databrary.HTTP.Form.Deform
import Databrary.HTTP.Path.Parser
import Databrary.Controller.Permission
import Databrary.Controller.Form
import Databrary.View.Periodic

viewPeriodic :: ActionRoute ()
viewPeriodic = action GET ("admin" >/> "periodic") $ \() -> withAuth $ do
  checkMemberADMIN
  peeks $ blankForm . htmlPeriodic

postPeriodic :: ActionRoute ()
postPeriodic = action POST ("admin" >/> "periodic") $ \() -> withAuth $ do
  checkMemberADMIN
  t <- peeks servicePeriodic
  w <- runForm (Just htmlPeriodic) $ "weekly" .:> deform
  liftIO $ mapM_ (`throwTo` if w then PeriodWeekly else PeriodDaily) t
  return $ okResponse [] (maybe "no" (const "ok") t :: String)

