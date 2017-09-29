{-# LANGUAGE OverloadedStrings #-}
module Databrary.Web.All
  ( generateAllJS
  , generateAllCSS
  ) where

import Control.Monad (when, forM_)
import Foreign.Marshal.Alloc (allocaBytes)
import System.IO (withBinaryFile, IOMode(ReadMode, WriteMode), hPutChar, hGetBufSome, hPutBuf)

import Databrary.Web
import Databrary.Web.Types
import Databrary.Web.Generate
import Databrary.Web.Libs

generateMerged :: [WebFilePath] -> WebGenerator
generateMerged l fo@(f, _) = do
  webRegenerate
    (allocaBytes z $ \b ->
      withBinaryFile (webFileAbs f) WriteMode $ \h ->
        forM_ l $ \s -> do
          withBinaryFile (webFileAbs s) ReadMode $
            copy b h
          hPutChar h '\n')
    [] l fo
  where
  copy b h i = do
    n <- hGetBufSome i b z
    when (n > 0) $ do
      hPutBuf h b n
      copy b h i
  z = 32768

generateAllJS :: WebGenerator
generateAllJS = generateMerged (webDeps False ++ ["app.min.js"])

generateAllCSS :: WebGenerator
generateAllCSS = generateMerged (cssWebDeps False)
