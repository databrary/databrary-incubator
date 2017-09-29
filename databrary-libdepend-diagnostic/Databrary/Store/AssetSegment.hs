{-# LANGUAGE OverloadedStrings #-}
module Databrary.Store.AssetSegment
  ( assetSegmentTag
  , getAssetSegmentStore
  ) where

import Control.Monad (unless, liftM2)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.ByteString.Lazy.Internal (defaultChunkSize)
import Data.Fixed (showFixed, Milli)
import Data.Maybe (isJust, fromMaybe, fromJust)
import Data.Monoid ((<>))
import Data.Word (Word16)
import qualified Data.Streaming.Process as P
import qualified Database.PostgreSQL.Typed.Range as Range
import System.IO (Handle, hClose)
import System.Posix.FilePath (takeDirectory)
import System.Posix.Files.ByteString (setFileMode)

import Databrary.Ops
import Databrary.Has
import Databrary.Files
import Databrary.Model.Offset
import Databrary.Model.Format
import Databrary.Model.Asset
import Databrary.Model.AssetSlot
import Databrary.Model.AssetSegment
import Databrary.Store.Types
import Databrary.Store.Asset
import Databrary.Store.Temp
import Databrary.Store.AV
import Databrary.Action.Types

assetSegmentTag :: AssetSegment -> Maybe Word16 -> String
assetSegmentTag as sz = m ':' (assetSegmentFull as ?!> s) ++ m '@' (show <$> sz) where
  m = maybe "" . (:)
  c = assetSegmentRange as
  s = maybe (b (Range.lowerBound c) ++ '-' : b (Range.upperBound c)) (show . offsetMillis) (Range.getPoint c)
  b = maybe "" (show . offsetMillis) . Range.bound

assetSegmentFile :: AssetSegment -> Maybe Word16 -> Maybe RawFilePath
assetSegmentFile as sz = (<> BSC.pack (assetSegmentTag as sz)) <$> assetFile (slotAsset $ segmentAsset as)

type Stream = BS.ByteString -> IO ()

stream :: Stream -> Handle -> IO ()
stream s h = loop where
  loop = do
    b <- BS.hGetSome h defaultChunkSize
    s b
    unless (BS.null b) $ loop

genVideoClip :: AV -> RawFilePath -> Maybe (Range.Range Offset) -> Maybe Word16 -> Either Stream RawFilePath -> IO ()
genVideoClip _ src (Just clip) _ dst | Nothing <- Range.getPoint clip =
  P.withCheckedProcess (P.proc "ffmpeg" $
    [ "-y", "-accurate_seek"
    , "-loglevel", "error"
    , "-threads", "1"
    , "-ss", sb lb
    , "-i", toFilePath src ]
    ++ maybe [] (\u -> ["-t", sb $ u - lb]) ub ++
    [ "-codec", "copy"
    , "-f", "mp4"
    , either (const "-") toFilePath dst ])
    { P.std_out = P.CreatePipe
    , P.close_fds = True
    }
    (\P.ClosedStream h P.Inherited ->
      either stream (const hClose) dst h)
  where
  lb = fromMaybe 0 $ Range.bound $ Range.lowerBound clip
  ub = Range.bound $ Range.upperBound clip
  sb = (showFixed True :: Milli -> String) . offsetMilli
genVideoClip av src frame sz dst =
  avFrame src (offsetDiffTime <$> (Range.getPoint =<< frame)) sz Nothing (rightJust dst) av
    >>= mapM_ (\b -> send b >> send BS.empty)
  where send = either id (const $ const $ return ()) dst

getAssetSegmentStore :: AssetSegment -> Maybe Word16 -> ActionM (Either (Stream -> IO ()) RawFilePath)
getAssetSegmentStore as sz
  | aimg && isJust sz || not (assetSegmentFull as) && isJust (assetDuration $ assetRow a) && isJust (formatSample afmt) = do
  Just af <- getAssetFile a
  av <- peek
  store <- peek
  rs <- peek
  let cache = storageCache store
      cf = liftM2 (</>) cache $ assetSegmentFile as sz
      gen = genVideoClip av af (aimg ?!> clip) sz
  liftIO $ maybe
    (return $ Left $ gen . Left)
    (\f -> do
      fe <- fileExist f
      unless fe $ do
        tf <- makeTempFileAs (maybe (storageTemp store) (</> "tmp/") cache) (const $ return ()) rs
        gen (Right (tempFilePath tf))
        _ <- createDir (takeDirectory f) 0o770
        setFileMode (tempFilePath tf) 0o640
        renameTempFile tf f rs
      return $ Right f)
    cf
  | otherwise = Right . fromJust <$> getAssetFile a
  where
  a = slotAsset $ segmentAsset as
  afmt = assetFormat $ assetRow a
  aimg = afmt == imageFormat
  clip = assetSegmentRange as
