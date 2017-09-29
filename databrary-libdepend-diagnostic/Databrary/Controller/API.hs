{-# LANGUAGE OverloadedStrings #-}
module Databrary.Controller.API
  ( viewSwagger
  ) where

import Databrary.Has
import Databrary.HTTP.Path.Parser
import Databrary.Action
import Databrary.Routes.API (swagger)

viewSwagger :: ActionRoute API
viewSwagger = action GET (pathAPI </< "swagger") $ \api -> withoutAuth $
  case api of
    HTML -> peeks notFoundResponse
    JSON -> return $ okResponse [("Access-Control-Allow-Origin","*")] swagger
