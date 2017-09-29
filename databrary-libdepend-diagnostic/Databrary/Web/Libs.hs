{-# LANGUAGE OverloadedStrings #-}
module Databrary.Web.Libs
  ( generateLib
  , webDeps
  , cssWebDeps
  , webLibs
  , webIncludes
  ) where

import Control.Monad (mzero)
import Data.Maybe (maybeToList)
import Data.List (stripPrefix)
import Data.String (fromString)
import System.FilePath ((</>), splitFileName, (<.>))

import Databrary.Web
import Databrary.Web.Types
import Databrary.Web.Generate

prefix :: FilePath
prefix = "bower_components"

jsDeps, jsIncludes, jsAll :: [(FilePath, FilePath)]
jsDeps = -- included in all
  [ ("jquery",              "jquery/dist")
  , ("angular",             "angular")
  , ("angular-route",       "angular-route")
  , ("ng-flow-standalone",  "ng-flow/dist")
  , ("pivot",               "pivottable/dist")
  , ("lodash",              "lodash")
  ]
jsIncludes = -- included in app (along with our js)
  map (\n -> ("jquery.ui." ++ n, "jquery-ui/ui")) ["core", "widget", "mouse", "slider", "sortable"] ++
  [ ("slider",              "angular-ui-slider/src")
  ]
jsAll = jsDeps ++ jsIncludes

extensions :: [FilePath]
extensions = ["js", "min.js", "min.map", "min.js.map", "css", "min.css"]

generateLib :: WebGenerator
generateLib fo@(f, _)
  | ("lib/", l) <- splitFileName (webFileRel f)
  , [p] <- [ p | (b, p) <- jsAll, ('.':e) <- maybeToList (stripPrefix b l), e `elem` extensions ] =
    webLinkDataFile (prefix </> p </> l) fo
  | otherwise = mzero

webJS :: Bool -> [(FilePath, FilePath)] -> [WebFilePath]
webJS mn = map (fromString . ("lib" </>) . (<.> if mn then ".min.js" else ".js") . fst)

webDeps :: Bool -> [WebFilePath]
webDeps debug = webJS (not debug) jsDeps

cssWebDeps :: Bool -> [WebFilePath]
cssWebDeps debug = map (fromString . (<.> if debug then ".css" else ".min.css")) ["lib/pivot", "app"]

webLibs :: [WebFilePath]
webLibs = webJS True jsDeps ++ ["lib/pivot.css"]

webIncludes :: [WebFilePath]
webIncludes = webJS False jsIncludes
