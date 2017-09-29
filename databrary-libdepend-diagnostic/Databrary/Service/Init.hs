{-# LANGUAGE OverloadedStrings, TemplateHaskell, RecordWildCards #-}
module Databrary.Service.Init
  ( withService
  ) where

import Control.Exception (bracket)
import Control.Monad (when, void)
import Data.IORef (newIORef)
import Data.Time.Clock (getCurrentTime)

import Databrary.Ops
import qualified Databrary.Store.Config as C
import Databrary.Service.DB (initDB, finiDB, runDBM)
import Databrary.Service.Entropy (initEntropy)
import Databrary.HTTP.Client (initHTTPClient)
import Databrary.Store.Service (initStorage)
import Databrary.Store.AV (initAV)
import Databrary.Service.Passwd (initPasswd)
import Databrary.Service.Log (initLogs, finiLogs)
import Databrary.Service.Messages (loadMessages)
import Databrary.Web.Service (initWeb)
import Databrary.Static.Service (initStatic)
import Databrary.Ingest.Service (initIngest)
import Databrary.Model.Stats
import Databrary.Solr.Service (initSolr, finiSolr)
import Databrary.EZID.Service (initEZID)
import Databrary.Service.Notification
import Databrary.Service.Periodic (forkPeriodic)
import Databrary.Service.Types
import Databrary.Controller.Notification (forkNotifier)

initService :: Bool -> C.Config -> IO Service
initService fg conf = do
  time <- getCurrentTime
  logs <- initLogs (conf C.! (if fg then "log" else "log.bg"))
  entropy <- initEntropy
  passwd <- initPasswd
  messages <- loadMessages
  db <- initDB (conf C.! "db")
  storage <- initStorage (conf C.! "store")
  av <- initAV
  web <- initWeb
  httpc <- initHTTPClient
  static <- initStatic (conf C.! "static")
  solr <- initSolr fg (conf C.! "solr")
  ezid <- initEZID (conf C.! "ezid")
  ingest <- initIngest
  notify <- initNotifications (conf C.! "notification")
  stats <- if fg then runDBM db lookupSiteStats else return (error "siteStats")
  statsref <- newIORef stats
  let rc = Service
        { serviceStartTime = time
        , serviceSecret = Secret $ conf C.! "secret"
        , serviceEntropy = entropy
        , servicePasswd = passwd
        , serviceLogs = logs
        , serviceMessages = messages
        , serviceDB = db
        , serviceStorage = storage
        , serviceAV = av
        , serviceWeb = web
        , serviceHTTPClient = httpc
        , serviceStatic = static
        , serviceStats = statsref
        , serviceIngest = ingest
        , serviceSolr = solr
        , serviceEZID = ezid
        , servicePeriodic = Nothing
        , serviceNotification = notify
        , serviceDown = conf C.! "store.DOWN"
        }
  periodic <- fg ?$> forkPeriodic rc
  when fg $ void $ forkNotifier rc
  return $! rc
    { servicePeriodic = periodic
    }

finiService :: Service -> IO ()
finiService Service{..} = do
  finiSolr serviceSolr
  finiDB serviceDB
  finiLogs serviceLogs

withService :: Bool -> C.Config -> (Service -> IO a) -> IO a
withService fg c = bracket (initService fg c) finiService
