{-# LANGUAGE OverloadedStrings #-}
module Databrary.Store.Transcoder
  ( runTranscoder
  , initTranscoder
  , transcodeEnabled
  ) where

import Data.Maybe (isJust)
import Data.Version (showVersion)
import System.Process (readProcessWithExitCode)
import System.Exit (ExitCode(..))

import Paths_databrary (version, getDataFileName)
import qualified Databrary.Store.Config as C
import Databrary.Store.Types

runTranscoder :: Transcoder -> [String] -> IO (ExitCode, String, String)
runTranscoder (Transcoder cmd arg) args =
  readProcessWithExitCode cmd (arg ++ args) ""

initTranscoder :: C.Config -> IO (Maybe Transcoder)
initTranscoder conf =
  case (host, dir) of
    (Nothing, Nothing) -> return Nothing
    _ -> Just <$> do
      cmd <- getDataFileName "transctl.sh"
      let t = Transcoder cmd $
                [ "-v", showVersion version ]
                ++ maybe [] (\d -> ["-d", d]) dir
                ++ maybe [] (\h -> ["-h", h]) host
                ++ maybe [] (\m -> ["-m", m]) mount
      (r, out, err) <- runTranscoder t ["-t"]
      case r of
        ExitSuccess -> return t
        ExitFailure e -> fail $ "initTranscoder test: " ++ show e ++ "\n" ++ out ++ err
  where
  host = conf C.! "host"
  dir = conf C.! "dir"
  mount = conf C.! "mount"

transcodeEnabled :: Storage -> Bool
transcodeEnabled = isJust . storageTranscoder
