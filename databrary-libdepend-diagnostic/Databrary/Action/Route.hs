
{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module Databrary.Action.Route
  ( Method(..)
  , ActionRoute
  , actionURL
  , actionURI
  , actionMethod
  , action
  , multipartAction
  , API(..)
  , pathHTML
  , pathJSON
  , pathAPI
  ) where

import qualified Data.ByteString.Builder as BSB
import qualified Data.Invertible as I
import Network.HTTP.Types (Query)
import Network.URI (URI(..))
import qualified Web.Route.Invertible as R
import Web.Route.Invertible (Method(..))

import Databrary.HTTP.Request
import Databrary.HTTP.Route
import Databrary.HTTP.Path.Parser
import Databrary.Action.Run

{- 
  Specify the action to take for a given route, often used as an infix operator between the route 
  specification and the function used to produce the result (which usually generates the HTTP response, but could be anything).

  data RouteAction a b = RouteAction
  { actionRoute :: !(Route a)
  , routeAction :: !(a -> b)
  }
 
  so RouteAction constructs and object that has a route and what to do with the route

-}
type ActionRoute a = R.RouteAction a Action

-- get the url for an action route
-- request, (routeaction (route action)), route, query -> bsb.builder
actionURL :: Maybe Request -> R.RouteAction r a -> r -> Query -> BSB.Builder
actionURL req r a q
  -- if the method is get then return the url after transforming request, route request and query
  | R.requestMethod rr == GET = routeURL req rr q
  | otherwise = error $ "actionURL: " ++ show rr
  -- get the route request
  where rr = R.requestActionRoute r a

-- same as above
actionURI :: Maybe Request -> R.RouteAction r a -> r -> Query -> URI
actionURI req r a q
  | R.requestMethod rr == GET = routeURI req rr q
  | otherwise = error $ "actionURI: " ++ show rr
  where rr = R.requestActionRoute r a

-- get action method i.e. GET or POST or w/e
actionMethod :: R.RouteAction r a -> r -> Method
actionMethod r = R.requestMethod . R.requestActionRoute r

action :: Method -> PathParser r -> (r -> a) -> R.RouteAction r a
action m p a = R.routePath p R.>* R.routeMethod m `R.RouteAction` a

multipartAction :: R.RouteAction q a -> R.RouteAction q a
multipartAction (R.RouteAction r a) =
  R.RouteAction (r R.>* (R.routeAccept "multipart/form-data" R.>| R.unit)) a

data API
  = HTML
  | JSON
  deriving (Eq, Show)

-- always succeeds in matching, used as a default when "api" isn't detected
pathHTML :: PathParser ()
pathHTML = R.unit

-- json api has /api in it
pathJSON :: PathParser ()
pathJSON = "api"

pathAPI :: PathParser API
pathAPI = [I.biCase|Left () <-> JSON ; Right () <-> HTML|] R.>$< (pathJSON R.>|< pathHTML)
