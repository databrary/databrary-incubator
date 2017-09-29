{-# LANGUAGE OverloadedStrings, TemplateHaskell, TypeFamilies #-}
module Databrary.Model.GeoNames
  ( GeoName(..)
  , geoNameUS
  , parseGeoNameRef
  , lookupGeoName
  ) where

import Control.Monad (guard)
import qualified Data.ByteString.Char8 as BSC
import Data.Int (Int64)
import Data.List (stripPrefix)
import Data.Maybe (fromJust, fromMaybe, listToMaybe)
import qualified Data.Text as T
import qualified Network.HTTP.Client as HC

import Databrary.Has (makeHasRec)
import qualified Databrary.JSON as JSON
import Databrary.HTTP.Client
import Databrary.Model.Id.Types

type instance IdType GeoName = Int64

data GeoName = GeoName
  { geoNameId :: !(Id GeoName)
  , geoName :: !T.Text
  }

makeHasRec ''GeoName ['geoNameId]

geoNameUS :: GeoName
geoNameUS = GeoName
  { geoNameId = Id 6252001
  , geoName = "United States"
  }

parseGeoNameRef :: String -> Maybe (Id GeoName)
parseGeoNameRef s = listToMaybe $ do
  (i, r) <- reads $ fromMaybe s (stripPrefix "http://sws.geonames.org/" s)
  guard (null r || r == "/")
  return $ Id i

parseGeoName :: JSON.Value -> JSON.Parser GeoName
parseGeoName = JSON.withObject "geoname" $ \j -> do
  i <- j JSON..: "geonameId"
  n <- j JSON..: "name"
  return GeoName
    { geoNameId = Id i
    , geoName = n
    }

geoNameReq :: HC.Request
geoNameReq = (fromJust $ HC.parseUrl "http://api.geonames.org/getJSON")
  { HC.cookieJar = Nothing }

lookupGeoName :: Id GeoName -> HTTPClient -> IO (Maybe GeoName)
lookupGeoName (Id i) hcm = do
  j <- httpRequestJSON req hcm
  return $ JSON.parseMaybe parseGeoName =<< j
  where req = HC.setQueryString [("geonameId", Just $ BSC.pack $ show i), ("username", Just "databrary")] geoNameReq
