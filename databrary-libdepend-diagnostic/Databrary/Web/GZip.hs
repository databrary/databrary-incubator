{-# LANGUAGE OverloadedStrings #-}
module Databrary.Web.GZip
  ( generateGZip
  ) where

import qualified Codec.Compression.GZip as GZ
import Control.Monad (mzero)
import qualified Data.ByteString.Lazy as BSL
import System.FilePath (takeExtension)

import Databrary.Files
import Databrary.Web
import Databrary.Web.Types
import Databrary.Web.Generate

generateGZip :: WebGenerator
generateGZip fo@(f, _)
  | (b, ".gz") <- splitWebExtension f
  , takeExtension (webFileRel b) `notElem` [".png"] -- things that don't compress
  = webRegenerate
      (BSL.writeFile (toFilePath f) . GZ.compress =<< BSL.readFile (toFilePath b))
      [] [b] fo
  | otherwise = mzero
