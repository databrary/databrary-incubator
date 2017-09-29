{-# LANGUAGE OverloadedStrings #-}
module Databrary.Controller.Web
  ( StaticPath(..)
  , staticPath
  , webFile
  ) where

import Data.ByteArray.Encoding (convertToBase, Base(Base16))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.Char (isAscii, isAlphaNum, toLower)
import qualified Data.Invertible as I
import Data.Maybe (isJust)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.HTTP.Types (notFound404, hContentEncoding)
import qualified Network.Wai as Wai
import System.Posix.FilePath (joinPath, splitDirectories)
import qualified Web.Route.Invertible as R

import Databrary.Ops
import Databrary.Has
import Databrary.Files
import Databrary.Model.Format
import Databrary.Action.Route
import Databrary.Action.Run
import Databrary.Action.Response
import Databrary.Action
import Databrary.HTTP
import Databrary.HTTP.Request
import Databrary.HTTP.File
import Databrary.HTTP.Path.Parser
import Databrary.Web.Types
import Databrary.Web.Cache

newtype StaticPath = StaticPath { staticFilePath :: RawFilePath }

ok :: Char -> Bool
ok '.' = True
ok '-' = True
ok '_' = True
ok c = isAscii c && isAlphaNum c

bsLCEq :: BS.ByteString -> BS.ByteString -> Bool
bsLCEq t s
  | BS.length t == BS.length s = t == BSC.map toLower s
  | otherwise = False

staticPath :: [BS.ByteString] -> StaticPath
staticPath = StaticPath . joinPath . map component where
  component c
    | not (BS.null c) && BSC.head c /= '.' && BSC.all ok c = c
    | otherwise = error ("staticPath: " ++ BSC.unpack c)

parseStaticPath :: [T.Text] -> Maybe StaticPath
parseStaticPath = fmap (StaticPath . joinPath) . mapM component where
  component c = TE.encodeUtf8 c <? (not (T.null c) && T.head c /= '.' && T.all ok c)

pathStatic :: PathParser (Maybe StaticPath)
pathStatic = (parseStaticPath I.:<->: maybe [] (map TE.decodeLatin1 . splitDirectories . staticFilePath)) >$< R.manyI R.parameter

webFile :: ActionRoute (Maybe StaticPath)
webFile = action GET ("web" >/> pathStatic) $ \sp -> withoutAuth $ do
  StaticPath p <- maybeAction sp
  (wf, wfi) <- either (\e -> result =<< if null e then peeks notFoundResponse else return $ response notFound404 [] (T.pack e)) return
    =<< focusIO (lookupWebFile p)
  agz <- any (bsLCEq "gzip") . concatMap splitHTTP <$> peeks (lookupRequestHeaders "accept-encoding")
  wgz <- if agz then rightJust <$> focusIO (lookupWebFile (p <.> ".gz")) else return Nothing
  r <- serveFile (toRawFilePath $ maybe wf fst wgz) (unknownFormat{ formatMimeType = webFileFormat wfi }) Nothing (convertToBase Base16 $ webFileHash wfi)
  return $ if isJust wgz then Wai.mapResponseHeaders ((hContentEncoding, "gzip") :) r else r
