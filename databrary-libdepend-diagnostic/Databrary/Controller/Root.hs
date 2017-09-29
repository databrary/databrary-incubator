{-# LANGUAGE CPP, OverloadedStrings #-}
module Databrary.Controller.Root
  ( viewRoot
  , viewConstants
  , viewRobotsTxt
  ) where

import Control.Monad (when)
import qualified Data.Aeson.Types as JSON
import Data.Maybe (isNothing)
import qualified Data.Text as T

import Databrary.Has
import qualified Databrary.JSON as JSON
import Databrary.Service.Types
import Databrary.HTTP.Path.Parser
import Databrary.Action
import Databrary.Controller.Angular
import Databrary.View.Root
import Databrary.Web.Constants

viewRoot :: ActionRoute API
viewRoot = action GET pathAPI $ \api -> withAuth $ do
  down <- peeks serviceDown
  when (api == HTML && isNothing down) angular
  case api of
    JSON -> return $ okResponse [] JSON.emptyObject
    HTML -> peeks $ okResponse [] . maybe htmlRoot htmlDown down

viewConstants :: ActionRoute ()
viewConstants = action GET (pathJSON >/> "constants") $ \() -> withoutAuth $
  return $ okResponse [] $ JSON.objectEncoding constantsJSON

viewRobotsTxt :: ActionRoute ()
viewRobotsTxt = action GET "robots.txt" $ \() -> withoutAuth $
  return $ okResponse [] (
#if defined(DEVEL) || defined(SANDBOX)
    "User-agent: *\nDisallow: /\n"
#else
    ""
#endif
    :: T.Text)
