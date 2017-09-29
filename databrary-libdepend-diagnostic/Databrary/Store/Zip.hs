{-# LANGUAGE RecordWildCards, OverloadedStrings #-}
module Databrary.Store.Zip
  ( ZipEntryContent(..)
  , ZipEntry(..)
  , blankZipEntry
  , sizeZip
  , streamZip
  , writeZipFile
  , fileZipEntry
  ) where

import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.RWS.Strict (RWST, execRWST, ask, local, tell)
import Control.Monad.State.Class (MonadState, get, modify')
import Control.Monad.State.Strict (execStateT)
import Data.Bits (bit, shiftL, (.|.))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Builder.Extra as B (defaultChunkSize)
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import Data.Digest.CRC32 (crc32, crc32Update)
import Data.List (foldl')
import Data.Maybe (isJust, fromMaybe)
import Data.Monoid ((<>))
import Data.Time.Calendar (toGregorian)
import Data.Time.Clock (UTCTime(..), getCurrentTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Time.LocalTime (TimeOfDay(..), timeToTimeOfDay)
import Data.Word (Word32, Word64)
import System.IO (withBinaryFile, IOMode(ReadMode, WriteMode))
import System.IO.Error (mkIOError, eofErrorType)
import System.Posix.Directory.Foreign (dtDir, dtReg)
import System.Posix.Directory.Traversals (getDirectoryContents)
import System.Posix.Files.ByteString (isDirectory, modificationTimeHiRes, fileSize)

import Databrary.Ops
import Databrary.Files

data ZipEntryContent
  = ZipDirectory [ZipEntry]
  | ZipEntryPure BSL.ByteString
  | ZipEntryFile
    { zipEntryFileSize :: Word64
    , zipEntryFilePath :: RawFilePath
    }
  deriving (Show, Eq)

type ZipPath = BS.ByteString

data ZipEntry = ZipEntry
  { zipEntryName :: ZipPath
  , zipEntryTime :: Maybe UTCTime
  , zipEntryComment :: BS.ByteString
  , zipEntryContent :: ZipEntryContent
  } deriving (Show, Eq)

blankZipEntry :: ZipEntry
blankZipEntry = ZipEntry
  { zipEntryName = BS.empty
  , zipEntryTime = Nothing
  , zipEntryComment = BS.empty
  , zipEntryContent = ZipEntryPure BSL.empty
  }

zip64Size :: Word64
zip64Size = 0xffffffff

infixr 1 ?=
(?=) :: Num a => Bool -> a -> a
True ?= x = x
False ?= _ = 0

zip64Ext :: B.Builder
zip64Ext = B.word16LE 1

zipSize :: Word64 -> B.Builder
zipSize = B.word32LE . fromIntegral . min zip64Size

zipTime :: UTCTime -> B.Builder
zipTime UTCTime{..} = B.word16LE time <> B.word16LE date where
  time = fromIntegral todHour `shiftL` 11 .|. fromIntegral todMin `shiftL` 5 .|. ceiling todSec `div` 2
  TimeOfDay{..} = timeToTimeOfDay utctDayTime
  date = fromIntegral (year - 1980) `shiftL` 9 .|. fromIntegral month `shiftL` 5 .|. fromIntegral day
  (year, month, day) = toGregorian utctDay

zipVersion :: Bool -> B.Builder
zipVersion False = B.word16LE 20
zipVersion True = B.word16LE 45

zipFlags :: Bool -> B.Builder
zipFlags False = B.word16LE $ bit 3
zipFlags True = B.word16LE 0

twice :: Monoid m => m -> m
twice m = m <> m

data ZipCEntry = ZipCEntry
  { zipCEntryPath :: !ZipPath
  , zipCEntryTime :: !UTCTime
  , zipCEntryKnown :: !Bool
  , zipCEntryCRC32 :: !Word32
  , zipCEntrySize :: !Word64
  , zipCEntryOffset :: !Word64
  , zipCEntry :: !ZipEntry
  }

data ZipSize = ZipSize
  { zipSizeCount :: !Int
  , zipSizeData, zipSizeDirectory :: !Word64
  }

instance Monoid ZipSize where
  mempty = ZipSize 0 0 0
  mappend (ZipSize a1 b1 c1) (ZipSize a2 b2 c2) = ZipSize (a1+a2) (b1+b2) (c1+c2)
  mconcat l = ZipSize (sum $ map zipSizeCount l) (sum $ map zipSizeData l) (sum $ map zipSizeDirectory l)

sizeZip :: [ZipEntry] -> Word64
sizeZip entries = len + (z64 ?= 76) + 22 where
  zsize = sizeZipEntries 0 mempty entries
  len = zipSizeData zsize + zipSizeDirectory zsize
  z64 = len >= zip64Size || zipSizeCount zsize >= 0xffff
  slash (ZipDirectory _) = succ
  slash _ = id
  sizeZipEntries = foldl' . sizeZipEntry
  sizeZipEntry path' zs ZipEntry{..} =
    case zipEntryContent of
      ZipDirectory l ->
        sizeZipEntries path (header True 0) l
      ZipEntryPure b ->
        header True $ fromIntegral (BSL.length b)
      ZipEntryFile size _ ->
        header False size
    where
    path = slash zipEntryContent $ path' + fromIntegral (BS.length zipEntryName)
    header known size = zs <> ZipSize 1
      (30 + path + (if known then size64 ?= 20 else if size64 then 24 else 16) + size)
      (46 + path + (size64 || off64 ?= 4 + (size64 ?= 16) + (off64 ?= 8)) + fromIntegral (BS.length zipEntryComment))
      where size64 = size >= zip64Size
    off64 = zipSizeData zs >= zip64Size

streamZip :: [ZipEntry] -> BS.ByteString -> (B.Builder -> IO ()) -> IO ()
streamZip entries comment write = do
  t <- getCurrentTime
  (off, centries) <- execRWST (streamZipEntries entries) (BS.empty, t) 0
  csize <- execStateT (mapM_ streamZipCEntry centries) 0
  let len = off + csize
      count = length centries
  when (len >= zip64Size || count >= 0xffff) $ write
    $ B.word32LE 0x06064b50
    <> B.word64LE 44 -- length of this record
    <> B.word16LE 63
    <> zipVersion True
    <> B.word32LE 0 -- disk
    <> B.word32LE 0 -- central disk
    <> twice (B.word64LE $ fromIntegral count)
    <> B.word64LE csize
    <> B.word64LE off
    <> B.word32LE 0x07064b50 -- locator:
    <> B.word32LE 0 -- central disk
    <> B.word64LE len
    <> B.word32LE 1 -- total disks
  write $ B.word32LE 0x06054b50
    <> B.word16LE 0 -- disk
    <> B.word16LE 0 -- central disk
    <> twice (B.word16LE $ fromIntegral $ min 0xffff count)
    <> zipSize csize
    <> zipSize off
    <> B.word16LE (fromIntegral $ BS.length comment)
    <> B.byteString comment
  where
  slash (ZipDirectory _) p = BSC.snoc p '/'
  slash _ p = p
  send :: (MonadState Word64 m, MonadIO m) => Word64 -> B.Builder -> m ()
  send l b = do
    modify' (fromIntegral l +)
    liftIO $ write b
  streamZipEntries = mapM_ streamZipEntry
  streamZipEntry :: ZipEntry -> RWST (BS.ByteString, UTCTime) [ZipCEntry] Word64 IO ()
  streamZipEntry z@ZipEntry{..} = do
    (path', time') <- ask
    off <- get
    let path = slash zipEntryContent $ path' <> zipEntryName
        time = fromMaybe time' zipEntryTime
        central k c s = tell [ZipCEntry
          { zipCEntryPath = path
          , zipCEntryTime = time
          , zipCEntryKnown = k
          , zipCEntryCRC32 = c
          , zipCEntrySize = s
          , zipCEntryOffset = off
          , zipCEntry = z
          }]
        header desc = do
          send (30 + fromIntegral (BS.length path) + fromIntegral el + s)
            $ B.word32LE 0x04034b50
            <> zipVersion (z64 || off >= zip64Size)
            <> zipFlags known
            <> B.word16LE 0 -- compression
            <> zipTime time
            <> B.word32LE c
            <> twice (zipSize s)
            <> B.word16LE (fromIntegral $ BS.length path)
            <> B.word16LE el
            <> B.byteString path
            <> (if z64 then zip64Ext <> B.word16LE 16 <> twice (B.word64LE s) else mempty)
          mapM_ (uncurry $ central True) desc
          where
          known = isJust desc
          (c, s) = fromMaybe (0, 0) desc
          z64 = s >= zip64Size
          el = z64 ?= 20
    case zipEntryContent of
      ZipDirectory l -> do
        header $ Just (0, 0)
        local (\_ -> (path, time)) $ streamZipEntries l
      ZipEntryPure b -> do
        header $ Just (crc32 b, fromIntegral $ BSL.length b)
        liftIO $ write $ B.lazyByteString b
      ZipEntryFile size f -> do
        header Nothing
        let run c 0 _ = return c
            run c s h = do
              b <- BS.hGetSome h (fromIntegral $ min s $ fromIntegral B.defaultChunkSize)
              when (BS.null b) $ ioError $ mkIOError eofErrorType "ZipEntryFile" (Just h) (Just $ toFilePath f)
              write $ B.byteString b
              run (crc32Update c b) (s - fromIntegral (BS.length b)) h
        c <- liftIO $ withBinaryFile (toFilePath f) ReadMode $ run 0 size
        modify' (size +)
        let s64 = size >= zip64Size
        send (if s64 then 24 else 16)
          $ B.word32LE 0x08074b50
          <> B.word32LE c
          <> twice ((if s64 -- this seems weird but it's what openjdk's ZipOutputStream does
            then B.word64LE else B.word32LE . fromIntegral) size)
        central False c size
  streamZipCEntry ZipCEntry{..} = do
    let z64 = e64 /= 0
        e64 = (zipCEntrySize >= zip64Size ?= 16) + (zipCEntryOffset >= zip64Size ?= 8)
        el = z64 ?= 4 + e64
    send (fromIntegral $ 46 + BS.length zipCEntryPath + fromIntegral el + BS.length (zipEntryComment zipCEntry))
      $ B.word32LE 0x02014b50
      <> B.word16LE 63 -- version
      <> zipVersion z64
      <> zipFlags zipCEntryKnown
      <> B.word16LE 0 -- compression
      <> zipTime zipCEntryTime
      <> B.word32LE zipCEntryCRC32
      <> twice (zipSize zipCEntrySize)
      <> B.word16LE (fromIntegral $ BS.length zipCEntryPath)
      <> B.word16LE el
      <> B.word16LE (fromIntegral $ BS.length $ zipEntryComment zipCEntry)
      <> B.word16LE 0 -- disk number
      <> B.word16LE 0 -- if text then bit 1
      <> B.word32LE 0
      <> zipSize zipCEntryOffset
      <> B.byteString zipCEntryPath
      <> (if z64
        then zip64Ext <> B.word16LE e64
          <> (if zipCEntrySize >= zip64Size then twice (B.word64LE zipCEntrySize) else mempty)
          <> (if zipCEntryOffset >= zip64Size then B.word64LE zipCEntryOffset else mempty)
        else mempty)
      <> B.byteString (zipEntryComment zipCEntry)

writeZipFile :: FilePath -> [ZipEntry] -> BS.ByteString -> IO ()
writeZipFile f e c =
  withBinaryFile f WriteMode $
    streamZip e c . B.hPutBuilder

fileZipEntry :: RawFilePath -> IO ZipEntry
fileZipEntry src = do
  s <- getFileStatus src
  let t = posixSecondsToUTCTime $ modificationTimeHiRes s
  if isDirectory s
    then dir src src (Just t)
    else return $ file' src src (fileSize s) t
  where
  dir d n t = do
    l <- getDirectoryContents d
    c <- mapMaybeM (ent d) l
    return ZipEntry
      { zipEntryName = n
      , zipEntryTime = t
      , zipEntryComment = BS.empty
      , zipEntryContent = ZipDirectory c
      }
  ent d (t,n)
    | n == "." || n == ".." = return Nothing
    | t == dtDir = Just <$> dir (d </> n) n Nothing
    | t == dtReg = Just <$> file (d </> n) n
    | otherwise = return Nothing
  file f n = do
    Just (s, t) <- fileInfo f
    return $ file' f n s t
  file' f n s t = ZipEntry
    { zipEntryName = n
    , zipEntryTime = Just t
    , zipEntryComment = BS.empty
    , zipEntryContent = ZipEntryFile (fromIntegral s) f
    }
