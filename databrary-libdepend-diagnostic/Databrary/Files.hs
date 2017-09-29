module Databrary.Files
  ( IsFilePath(..)
  , RawFilePath
  , makeRelative
  , catchDoesNotExist
  , modificationTimestamp
  , fileInfo
  , setFileTimestamps
  , removeFile
  , createDir
  , compareFiles
  , hashFile
  ) where

import Control.Arrow ((&&&))
import Control.Exception (handleJust)
import Control.Monad (guard, liftM2)
import Crypto.Hash (HashAlgorithm, hashInit, hashUpdate, hashFinalize, Digest)
import Data.ByteArray (MemView(..))
import qualified Data.ByteString as BS
import Data.ByteString.Lazy.Internal (defaultChunkSize)
import Data.Maybe (isJust)
import Data.String (IsString(..))
import Data.Time.Clock.POSIX (POSIXTime, posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import Foreign.Marshal.Alloc (allocaBytes)
import qualified GHC.Foreign as GHC
import GHC.IO.Encoding (getFileSystemEncoding)
import System.Posix.ByteString.FilePath (RawFilePath)
import qualified System.FilePath as F
import qualified System.Posix.FilePath as RF
import qualified System.Posix as P
import qualified System.Posix.ByteString as RP
import System.Posix.Types (FileMode)
import System.IO (withBinaryFile, IOMode(ReadMode), hGetBufSome)
import System.Posix.Types (FileOffset)
import System.IO.Error (isDoesNotExistError, isAlreadyExistsError)
import System.IO.Unsafe (unsafeDupablePerformIO)

import Databrary.Ops
import Databrary.Model.Time

rawFilePath :: FilePath -> RawFilePath
rawFilePath s = unsafeDupablePerformIO $ do
  enc <- getFileSystemEncoding
  GHC.withCStringLen enc s BS.packCStringLen

unRawFilePath :: RawFilePath -> FilePath
unRawFilePath b = unsafeDupablePerformIO $ do
  enc <- getFileSystemEncoding
  BS.useAsCStringLen b $ GHC.peekCStringLen enc

class IsString a => IsFilePath a where
  toFilePath :: a -> F.FilePath
  fromFilePath :: F.FilePath -> a
  fromFilePath = fromString
  toRawFilePath :: a -> RF.RawFilePath
  fromRawFilePath :: RF.RawFilePath -> a

  (</>) :: a -> a -> a
  (<.>) :: a -> a -> a

  fileExist :: a -> IO Bool
  fileExist = RP.fileExist . toRawFilePath
  getFileStatus :: a -> IO P.FileStatus
  getFileStatus = RP.getFileStatus . toRawFilePath
  setFileTimesHiRes :: a -> POSIXTime -> POSIXTime -> IO ()
  setFileTimesHiRes = RP.setFileTimesHiRes . toRawFilePath
  removeLink :: a -> IO ()
  removeLink = RP.removeLink . toRawFilePath
  createDirectory :: a -> FileMode -> IO ()
  createDirectory = RP.createDirectory . toRawFilePath

instance IsFilePath F.FilePath where
  toFilePath = id
  fromFilePath = id
  toRawFilePath = rawFilePath
  fromRawFilePath = unRawFilePath

  (</>) = (F.</>)
  (<.>) = (F.<.>)

  fileExist = P.fileExist
  getFileStatus = P.getFileStatus
  setFileTimesHiRes = P.setFileTimesHiRes
  removeLink = P.removeLink
  createDirectory = P.createDirectory

instance IsFilePath RF.RawFilePath where
  toFilePath = unRawFilePath
  fromFilePath = rawFilePath
  toRawFilePath = id
  fromRawFilePath = id

  (</>) = (RF.</>)
  (<.>) = (RF.<.>)

makeRelative :: IsFilePath a => a -> a -> a
makeRelative a b = fromFilePath $ F.makeRelative (toFilePath a) (toFilePath b)

catchOnlyIO :: (IOError -> Bool) -> IO a -> IO (Maybe a)
catchOnlyIO c f = handleJust (guard . c) (\_ -> return Nothing) $ Just <$> f

catchDoesNotExist :: IO a -> IO (Maybe a)
catchDoesNotExist = catchOnlyIO isDoesNotExistError

catchAlreadyExists :: IO a -> IO (Maybe a)
catchAlreadyExists = catchOnlyIO isAlreadyExistsError

modificationTimestamp :: P.FileStatus -> Timestamp
modificationTimestamp = posixSecondsToUTCTime . P.modificationTimeHiRes

fileInfo :: IsFilePath a => a -> IO (Maybe (FileOffset, Timestamp))
fileInfo f =
  (=<<) (liftM2 (?>) P.isRegularFile $ P.fileSize &&& modificationTimestamp)
  <$> catchDoesNotExist (getFileStatus f)

setFileTimestamps :: IsFilePath a => a -> Timestamp -> Timestamp -> IO ()
setFileTimestamps f a m = setFileTimesHiRes f (utcTimeToPOSIXSeconds a) (utcTimeToPOSIXSeconds m)

removeFile :: IsFilePath a => a -> IO Bool
removeFile f = isJust <$> catchDoesNotExist (removeLink f)

createDir :: IsFilePath a => a -> FileMode -> IO Bool
createDir f m = isJust <$> catchAlreadyExists (createDirectory f m)

compareFiles :: IsFilePath a => a -> a -> IO Bool
compareFiles f1 f2 = do
  s1 <- getFileStatus f1
  s2 <- getFileStatus f2
  if P.deviceID s1 == P.deviceID s2 && P.fileID s1 == P.fileID s2 then return True
    else if P.fileSize s1 /= P.fileSize s2 then return False
    else withBinaryFile (toFilePath f1) ReadMode $ withBinaryFile (toFilePath f2) ReadMode . cmp where
    cmp h1 h2 = do
      b1 <- BS.hGet h1 defaultChunkSize
      b2 <- BS.hGet h2 defaultChunkSize
      if b1 == b2
        then if BS.null b1 then return True else cmp h1 h2
        else return False

hashFile :: (IsFilePath f, HashAlgorithm a) => f -> IO (Digest a)
hashFile f =
  withBinaryFile (toFilePath f) ReadMode $ \h ->
    allocaBytes z $ \b ->
      run h b hashInit where
  run h b s = do
    n <- hGetBufSome h b z
    if n == 0
      then return $! hashFinalize s
      else run h b $! hashUpdate s (MemView b n)
  z = 32786
