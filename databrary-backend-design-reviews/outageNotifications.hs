{-
existing type:
module D.S.Types

data Service = Service
  { serviceStartTime :: !Timestamp
  ...
  , serviceDown :: !(Maybe T.Text)
  }
-}

type UserNoticeMessage = String -- notification message will be propagated to the front end for display.

-- | Mutually exclusive service levels
data ServiceLevel = 
     DegradedStorageDown UserNoticeMesage
   | DegradedTranscodingDown UserNoticeMessage
   | FullyOperational
  deriving (Ord)

data Service = Service
  { serviceStartTime :: !Timestamp
  ...
  , serviceLevel :: !ServiceLevel
  }

{- existing initialization logic:
module D.S.Init

initService
    :: Bool -- ^ Run in foreground?
    -> C.Config
    -> IO Service
initService fg conf = do
  time <- getCurrentTime
  ...
  storage <- initStorage (conf C.! "store")
  let rc = Service
        { serviceStartTime = time
        ...
        , serviceStorage = storage
        ...
        , serviceDown = conf C.! "store.DOWN"
        }
  return $! rc
    { servicePeriodic = periodic
    }

module D.Store.Service

initStorage :: C.Config -> IO Storage
initStorage conf
  | Just down <- conf C.! "DOWN" = return $ error $ "Storage unavailable: " ++ down
  | otherwise = do
      ...
      
      tc <- initTranscoder (conf C.! "transcode")

      return $ Storage
        { storageMaster = master
        ...
        , storageTranscoder = tc
        }
        
module D.Store.Transcoder

initTranscoder :: C.Config -> IO (Maybe Transcoder)
initTranscoder conf =
  ...
  Just <$> do
      ...
      (r, out, err) <- runTranscoder t ["-t"]
      case r of
        ExitSuccess -> return t
        ExitFailure e -> fail $ "initTranscoder test: " ++ show e ++ "\n" ++ out ++ err
-}

data TranscodingServiceLevel = TranscodingDown | FullyOperational
   deriving (Ord)

initStorage :: C.Config -> ServiceLevel -> IO Storage

-- initStorage will now branch on ServiceLevel.

initTranscoder :: C.Config -> TranscodingServiceLevel -> IO (Maybe Transcoder)
