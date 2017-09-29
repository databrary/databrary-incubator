{-# LANGUAGE OverloadedStrings #-}
module Databrary.Solr.Service
  ( Solr(..)
  , initSolr
  , finiSolr
  , MonadSolr
  ) where

import Control.Monad (when, forM_)
import Control.Monad.IO.Class (MonadIO)
import Data.Maybe (isNothing, fromMaybe)
import qualified Network.HTTP.Client as HC
import System.Directory (makeAbsolute, createDirectoryIfMissing, getDirectoryContents, copyFile)
import System.Environment (getEnvironment)
import System.FilePath ((</>))
import System.IO (withFile, openFile, IOMode(AppendMode, WriteMode), hPutStrLn)
import qualified System.Process as Proc
import System.Timeout (timeout)

import Paths_databrary (getDataFileName)
import Databrary.Ops
import Databrary.Has
import qualified Databrary.Store.Config as C
import Databrary.HTTP.Client (HTTPClient)
import Databrary.Model.Enum (pgEnumValues)
import Databrary.Model.Permission.Types
import Databrary.Model.Release.Types

data Solr = Solr
  { solrRequest :: HC.Request
  , solrProcess :: Maybe Proc.ProcessHandle
  }

confSolr :: FilePath -> FilePath -> IO ()
confSolr src dst = do
  mapM_ (\f -> when (head f /= '.') $ copyFile (src </> f) (dst </> f)) =<< getDirectoryContents src
  withFile (dst </> "enum.xml") WriteMode $ \h -> do
    hPutStrLn h "<?xml version=\"1.0\" encoding=\"UTF-8\" ?>"
    hPutStrLn h "<enumsConfig>"
    pe h "permission" PermissionNONE
    pe h "release" ReleasePRIVATE
    hPutStrLn h "</enumsConfig>"
  where
  pe h n t = do
    hPutStrLn h $ "<enum name=\"" ++ n ++ "\">"
    forM_ pgEnumValues $ \(x, s) -> hPutStrLn h $ "  <value>" ++ const s (x `asTypeOf` t) ++ "</value>"
    hPutStrLn h "</enum>"

initSolr :: Bool -> C.Config -> IO Solr
initSolr fg conf = do
  home <- makeAbsolute $ conf C.! "home"

  dir <- makeAbsolute =<< getDataFileName "solr"
  createDirectoryIfMissing True (home </> core </> "conf")
  copyFile (dir </> "solr.xml") (home </> "solr.xml")
  withFile (home </> core </> "core.properties") WriteMode $ \h ->
    hPutStrLn h $ "name=" ++ core
  confSolr (dir </> "conf") (home </> core </> "conf")

  env <- getEnvironment
  out <- maybe (return Proc.Inherit) (\f -> Proc.UseHandle <$> openFile f AppendMode) $ conf C.! "log"
  p <- fromMaybe fg (conf C.! "run") ?$> Proc.createProcess (Proc.proc (fromMaybe "solr" $ conf C.! "bin") ["start", "-Djetty.host=" ++ host, "-p", show port, "-f", "-s", home])
    { Proc.std_out = out
    , Proc.std_err = out
    , Proc.close_fds = True
    , Proc.env = Just $ env ++ [("SOLR_PID_DIR", home), ("LOG4J_PROPS", dir </> "log4j.properties")]
    , Proc.create_group = True
    }

  req <- HC.parseUrl $ "http://" ++ host ++ "/solr/" ++ core ++ "/"
  return Solr
    { solrRequest = req
      { HC.port = port
      , HC.redirectCount = 0
      , HC.cookieJar = Nothing
      }
    , solrProcess = (\(_,_,_,h) -> h) <$> p
    }
  where
  host = fromMaybe "127.0.0.1" $ conf C.! "host"
  port = conf C.! "port"
  core = fromMaybe "databrary" $ conf C.! "core"

finiSolr :: Solr -> IO ()
finiSolr Solr{ solrProcess = Just ph } = do
  Proc.interruptProcessGroupOf ph
  -- this timeout doesn't actually seem to work:
  r <- timeout 10000000 $ Proc.waitForProcess ph
  when (isNothing r) $ do
    putStrLn "solr failed to stop; terminating..."
    Proc.terminateProcess ph
finiSolr _ = return ()

type MonadSolr c m = (MonadIO m, MonadHas HTTPClient c m, MonadHas Solr c m)
