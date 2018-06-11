type UserNoticeMessage = String

-- | Mutually exclusive service levels
data ServiceLevel = 
     DegradedStorageDown UserNoticeMesage
   | DegradedTranscodingDown UserNoticeMessage
   | FullyOperational
  deriving (Ord)
 
 -- | 
 data TranscodingServiceLevel = TranscodingDown | FullyOperational
   deriving (Ord)
 
 data StorageServiceLevel = StorageDown | FullyOperational
   deriving (Ord)
 
 -- utiity used during transcoding init
 
 
 -- utility used during storage init
 
