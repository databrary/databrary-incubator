{-# LANGUAGE ExistentialQuantification, RecordWildCards, ImpredicativeTypes, GeneralizedNewtypeDeriving #-}
module Databrary.HTTP.Route
  ( Route
  , routeURL
  , routeURI
  ) where

import Prelude hiding (lookup)

import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Char8 as BSC
import Data.Monoid ((<>))
import Network.HTTP.Types (Query, simpleQueryToQuery, renderQuery)
import Network.URI (URI(..), nullURI)
import qualified Network.Wai as Wai
import qualified Web.Route.Invertible as R
import qualified Web.Route.Invertible.URI as R
import qualified Web.Route.Invertible.Internal as R

import Databrary.HTTP
import Databrary.HTTP.Request

type Route a r = R.RouteAction r a

routeURL :: Maybe Request -> R.Request -> Query -> BSB.Builder
routeURL w r q = bh (R.requestHost r)
  <> encodePath' (R.requestPath r)
    ((simpleQueryToQuery $ R.paramsQuerySimple $ R.requestQuery r) ++ q)
  where
  bh [] = foldMap (BSB.byteString . requestHost) w
  bh [x] = BSB.byteString x
  bh (x:l) = bh l <> BSB.char7 '.' <> BSB.byteString x

routeURI :: Maybe Wai.Request -> R.Request -> Query -> URI
routeURI req r q = (maybe nullURI requestURI req)
  { uriPath = uriPath ruri
  , uriQuery = BSC.unpack $ renderQuery True $ (simpleQueryToQuery $ R.paramsQuerySimple $ R.requestQuery r) ++ q
  } where
  ruri = R.requestURI r

