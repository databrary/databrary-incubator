{-# LANGUAGE OverloadedStrings #-}
module Databrary.Web.Templates
  ( generateTemplatesJS
  ) where

import Control.Monad (guard, unless, forM_)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Char8 as BSC
import Data.Char (isSpace)
import Data.Monoid ((<>))
import System.IO (withFile, withBinaryFile, IOMode(ReadMode, WriteMode), hPutStrLn, hIsEOF, hFlush)

import qualified Databrary.JSON as JSON
import Databrary.Files
import Databrary.Web
import Databrary.Web.Types
import Databrary.Web.Files
import Databrary.Web.Generate

processTemplate :: RawFilePath -> (BS.ByteString -> IO ()) -> IO ()
processTemplate f g = withFile (toFilePath f) ReadMode go where
  go h = do
    e <- hIsEOF h
    unless e $ do
      l <- BS.hGetLine h
      g $ BSC.dropWhile isSpace l
      go h

generateTemplatesJS :: WebGenerator
generateTemplatesJS fo@(f, _) = do
  tl <- liftIO $ findWebFiles ".html"
  guard (not $ null tl)
  webRegenerate
    (withBinaryFile (toFilePath f) WriteMode $ \h -> do
      hPutStrLn h "app.run(['$templateCache',function(t){"
      forM_ tl $ \tf -> do
        BSB.hPutBuilder h $ BSB.string8 "t.put(" <> JSON.quoteByteString q (webFileRelRaw tf) <> BSB.char8 ',' <> BSB.char8 q
        processTemplate (webFileAbsRaw tf) $ \s -> do
          let j = JSON.escapeByteString q s
          BSB.hPutBuilder h j -- this is hanging
          hFlush h            -- without this!!!
        hPutStrLn h $ q : ");"
      hPutStrLn h "}]);")
    [] tl fo
  where q = '\''
