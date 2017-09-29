{-# LANGUAGE OverloadedStrings #-}
module Databrary.Web.JSHint
  ( checkJSHint
  ) where

import Control.Monad (mzero, when)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Char8 as BSC
import System.FilePath (takeExtensions)
import System.Posix.FilePath (splitFileName, addExtension)
import System.Posix.IO.ByteString (openFd, OpenMode(WriteOnly), defaultFileFlags, closeFd)
import System.Process (callProcess)

import Paths_databrary.Node
import Databrary.Files
import Databrary.Web
import Databrary.Web.Types
import Databrary.Web.Generate

checkJSHint :: WebGenerator
checkJSHint fo@(f, _)
  | takeExtensions (webFileRel f) == ".js" = do
    r <- fileNewer f fo
    when r $ liftIO $ do
      ht <- fmap snd <$> fileInfo h
      ft <- modificationTimestamp <$> getFileStatus f
      when (all (ft >) ht) $ do
        callProcess (binDir </> "jshint") [webFileAbs f]
        maybe
          (openFd h WriteOnly (Just 0o666) defaultFileFlags >>= closeFd)
          (\_ -> setFileTimestamps h ft ft)
          ht
    return r
  | otherwise = mzero
  where
  (d, n) = splitFileName $ webFileAbsRaw f
  h = d </> ('.' `BSC.cons` n `addExtension` ".hinted")
