{-# LANGUAGE OverloadedStrings, TupleSections #-}
module Databrary.Web.Info
  ( makeWebFileInfo
  , loadWebFileMap
  ) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.HashMap.Strict as HM
import Data.Maybe (fromMaybe)
import System.FilePath (takeExtension)

import Databrary.Files
import Databrary.Model.Format
import Databrary.Web
import Databrary.Web.Files
import Databrary.Web.Types

staticFormats :: [(String, BS.ByteString)]
staticFormats = concatMap (\f -> map (\e -> ('.':BSC.unpack e, formatMimeType f)) $ formatExtension f) allFormats ++
  [ (".html", "text/html")
  , (".js", "application/javascript")
  , (".css", "text/css")
  , (".svg", "image/svg+xml")
  , (".json", "application/json")
  ]

makeWebFileInfo :: WebFilePath -> IO WebFileInfo
makeWebFileInfo f = WebFileInfo
  (fromMaybe "application/octet-stream" $ lookup (takeExtension $ webFileRel f) staticFormats)
  <$> hashFile (webFileAbsRaw f)
  <*> (modificationTimestamp <$> getFileStatus f)

loadWebFileMap :: IO WebFileMap
loadWebFileMap = fmap HM.fromList . mapM (\f -> (f, ) <$> makeWebFileInfo f) =<< allWebFiles
