{-# LANGUAGE ForeignFunctionInterface, DeriveDataTypeable, EmptyDataDecls, OverloadedStrings, NegativeLiterals, GeneralizedNewtypeDeriving #-}
module Databrary.Store.AV
  ( AVError(..)
  , avErrorString
  , AV
  , initAV
  , AVMediaType(..)
  , AVProbe(..)
  , avProbe
  , avProbeLength
  , avProbeHas
  , avFrame
  ) where

import Control.Applicative ((<|>))
import Control.Arrow (first)
import Control.Concurrent.MVar (MVar, newMVar, takeMVar, putMVar)
import Control.Exception (Exception, throwIO, bracket, bracket_, finally, onException)
import Control.Monad ((<=<), void, when, forM, forM_)
import qualified Data.ByteString as BS
import Data.Int (Int32, Int64)
import Data.Maybe (isNothing)
import Data.Ratio (Ratio, (%), numerator, denominator)
import Data.Time.Clock (DiffTime)
import Data.Time.Format (parseTimeM, defaultTimeLocale)
import Data.Time.LocalTime (ZonedTime)
import Data.Typeable (Typeable)
import Data.Word (Word16, Word32)
import Foreign.C.Error (throwErrnoIfNull)
import Foreign.C.String (CString, CStringLen, peekCAString, withCAString)
import Foreign.C.Types (CInt(..), CUInt(..), CSize(..))
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Marshal.Utils (with, maybePeek)
import Foreign.Ptr (Ptr, FunPtr, nullPtr, plusPtr, castPtr)
import Foreign.StablePtr
import Foreign.Storable
import System.IO.Unsafe (unsafeDupablePerformIO)

import Databrary.Ops
import Databrary.Files
import Databrary.Model.Offset

#include <libavformat/avformat.h>

type AVLockOp = #type enum AVLockOp
type AVLockmgr a = Ptr (Ptr a) -> AVLockOp -> IO CInt
type AVPixelFormat = #type enum AVPixelFormat
type AVCodecID = #type enum AVCodecID

data AVDictionary
data AVDictionaryEntry
data AVInputFormat
data AVFrame
data AVPacket
data AVCodec
data AVCodecContext
data AVStream
data AVFormatContext
data AVIOContext
data AVOutputFormat

foreign import ccall unsafe "libavutil/error.h av_strerror"
  avStrerror :: CInt -> CString -> CSize -> IO CInt

foreign import ccall unsafe "libavutil/mem.h av_free"
  avFree :: Ptr a -> IO ()

foreign import ccall unsafe "libavutil/dict.h av_dict_get"
  avDictGet :: Ptr AVDictionary -> CString -> Ptr AVDictionaryEntry -> CInt -> IO (Ptr AVDictionaryEntry)

foreign import ccall unsafe "libavutil/dict.h av_dict_set"
  avDictSet :: Ptr (Ptr AVDictionary) -> CString -> CString -> CInt -> IO CInt

foreign import ccall unsafe "libavutil/dict.h av_dict_free"
  avDictFree :: Ptr (Ptr AVDictionary) -> IO ()

foreign import ccall unsafe "libavutil/frame.h av_frame_alloc"
  avFrameAlloc :: IO (Ptr AVFrame)

foreign import ccall unsafe "libavutil/frame.h av_frame_free"
  avFrameFree :: Ptr (Ptr AVFrame) -> IO ()

foreign import ccall unsafe "libavutil/frame.h av_frame_unref"
  avFrameUnref :: Ptr AVFrame -> IO ()

foreign import ccall unsafe "libavutil/frame.h av_frame_get_best_effort_timestamp"
  avFrameGetBestEffortTimestamp :: Ptr AVFrame -> IO Int64

foreign import ccall "libavcodec/avcodec.h av_lockmgr_register"
  avLockmgrRegister :: FunPtr (AVLockmgr a) -> IO CInt

foreign import ccall "wrapper"
  mkAVLockmgr :: AVLockmgr a -> IO (FunPtr (AVLockmgr a))

foreign import ccall unsafe "libavcodec/avcodec.h avcodec_get_name"
  avcodecGetName :: AVCodecID -> IO CString

foreign import ccall unsafe "libavcodec/avcodec.h av_init_packet"
  avInitPacket :: Ptr AVPacket -> IO ()

foreign import ccall unsafe "libavcodec/avcodec.h av_free_packet"
  avFreePacket :: Ptr AVPacket -> IO ()

foreign import ccall "libavcodec/avcodec.h avcodec_open2"
  avcodecOpen2 :: Ptr AVCodecContext -> Ptr AVCodec -> Ptr (Ptr AVDictionary) -> IO CInt

foreign import ccall "libavcodec/avcodec.h avcodec_close"
  avcodecClose :: Ptr AVCodecContext -> IO CInt

foreign import ccall "libavcodec/avcodec.h avcodec_decode_video2"
  avcodecDecodeVideo2 :: Ptr AVCodecContext -> Ptr AVFrame -> Ptr CInt -> Ptr AVPacket -> IO CInt

foreign import ccall "libavcodec/avcodec.h avcodec_encode_video2"
  avcodecEncodeVideo2 :: Ptr AVCodecContext -> Ptr AVPacket -> Ptr AVFrame -> Ptr CInt -> IO CInt

foreign import ccall "libavcodec/avcodec.h avcodec_find_encoder"
  avcodecFindEncoder :: AVCodecID -> IO (Ptr AVCodec)

foreign import ccall "libavcodec/avcodec.h avcodec_find_best_pix_fmt_of_list"
  avcodecFindBestPixFmtOfList :: Ptr AVPixelFormat -> AVPixelFormat -> CInt -> Ptr CInt -> IO AVPixelFormat

foreign import ccall unsafe "libavformat/avio.h avio_open_dyn_buf"
  avioOpenDynBuf :: Ptr (Ptr AVIOContext) -> IO CInt

foreign import ccall unsafe "libavformat/avio.h avio_close_dyn_buf"
  avioCloseDynBuf :: Ptr AVIOContext -> Ptr CString -> IO CInt

foreign import ccall "libavformat/avformat.h av_register_all"
  avRegisterAll :: IO ()

foreign import ccall "libavformat/avformat.h avformat_open_input"
  avformatOpenInput :: Ptr (Ptr AVFormatContext) -> CString -> Ptr AVInputFormat -> Ptr (Ptr AVDictionary) -> IO CInt

foreign import ccall "libavformat/avformat.h avformat_close_input"
  avformatCloseInput :: Ptr (Ptr AVFormatContext) -> IO ()

foreign import ccall "libavformat/avformat.h avformat_find_stream_info"
  avformatFindStreamInfo :: Ptr AVFormatContext -> Ptr (Ptr AVDictionary) -> IO CInt

foreign import ccall unsafe "libavformat/avformat.h av_find_input_format"
  avFindInputFormat :: CString -> IO (Ptr AVInputFormat)

foreign import ccall "libavformat/avformat.h av_find_best_stream"
  avFindBestStream :: Ptr AVFormatContext -> #{type enum AVMediaType} -> CInt -> CInt -> Ptr (Ptr AVCodec) -> CInt -> IO CInt

foreign import ccall "libavformat/avformat.h avformat_seek_file"
  avformatSeekFile :: Ptr AVFormatContext -> CInt -> Int64 -> Int64 -> Int64 -> CInt -> IO CInt

foreign import ccall "libavformat/avformat.h av_read_frame"
  avReadFrame :: Ptr AVFormatContext -> Ptr AVPacket -> IO CInt

foreign import ccall "libavformat/avformat.h avformat_alloc_output_context2"
  avformatAllocOutputContext2 :: Ptr (Ptr AVFormatContext) -> Ptr AVOutputFormat -> CString -> CString -> IO CInt

foreign import ccall "libavformat/avformat.h avformat_free_context"
  avformatFreeContext :: Ptr AVFormatContext -> IO ()

foreign import ccall "libavformat/avformat.h avformat_new_stream"
  avformatNewStream :: Ptr AVFormatContext -> Ptr AVCodec -> IO (Ptr AVStream)

foreign import ccall "libavformat/avformat.h avformat_write_header"
  avformatWriteHeader :: Ptr AVFormatContext -> Ptr (Ptr AVDictionary) -> IO CInt

foreign import ccall "libavformat/avformat.h av_write_frame"
  avWriteFrame :: Ptr AVFormatContext -> Ptr AVPacket -> IO CInt

foreign import ccall "libavformat/avformat.h av_write_trailer"
  avWriteTrailer :: Ptr AVFormatContext -> IO CInt


newtype AVRational = AVRational (Ratio CInt) deriving (Eq, Ord, Num, Fractional, Real, RealFrac)

instance Storable AVRational where
  sizeOf _ = #{size AVRational}
  alignment (AVRational x) = alignment $ numerator x
  peek p = do
    num <- #{peek AVRational, num} p
    den <- #{peek AVRational, den} p
    return $ AVRational $ num % den
  poke p (AVRational x) = do
    #{poke AVRational, num} p (numerator x)
    #{poke AVRational, den} p (denominator x)


avShowError :: CInt -> String
avShowError e = unsafeDupablePerformIO $
  allocaBytes len $ \p -> do
    _ <- avStrerror e p (fromIntegral len)
    peekCAString p
  where len = 256

data AVError = AVError
  { avErrorCode :: CInt
  , avErrorFunction :: String
  , avErrorFile :: Maybe RawFilePath
  } deriving (Typeable)

instance Exception AVError

avErrorString :: AVError -> String
avErrorString = avShowError . avErrorCode

instance Show AVError where
  showsPrec p (AVError e c f) = showParen (p > 10) $
    showString "AVError "
    . showString c
    . maybe id (((' ' :) .) . shows) f
    . if e == 0 then id else
      showString ": " . showString (avShowError e)

data ErrorFile
  = NoFile
  | FileName !RawFilePath
  | FileContext !(Ptr AVFormatContext)

errorFile :: ErrorFile -> IO (Maybe RawFilePath)
errorFile NoFile = return Nothing
errorFile (FileName f) = return $ Just f
errorFile (FileContext a) =
  n /= nullPtr ?$> BS.packCString n
  where n = #{ptr AVFormatContext, filename} a

throwAVError :: CInt -> String -> ErrorFile -> IO a
throwAVError e c f = throwIO . AVError e c =<< errorFile f

throwAVErrorIf :: Integral a => String -> ErrorFile -> IO a -> IO a
throwAVErrorIf c f g = do
  r <- g
  when (r < 0) $
    throwAVError (fromIntegral r) c f
  return r

throwAVErrorIf_ :: Integral a => String -> ErrorFile -> IO a -> IO ()
throwAVErrorIf_ c f = void . throwAVErrorIf c f

throwAVErrorIfNull :: String -> ErrorFile -> IO (Ptr a) -> IO (Ptr a)
throwAVErrorIfNull c f g = do
  r <- g
  when (r == nullPtr) $
    throwAVError 0 c f
  return r


withAVDictionary :: (Ptr (Ptr AVDictionary) -> IO a) -> IO a
withAVDictionary f = with nullPtr $ \d ->
  f d `finally` avDictFree d

getAVDictionary :: Ptr AVDictionary -> String -> IO (Maybe String)
getAVDictionary dict key =
  withCAString key $ \ckey ->
    maybePeek (peekCAString <=< #{peek AVDictionaryEntry, value}) =<< avDictGet dict ckey nullPtr 0

setAVDictionary :: Ptr (Ptr AVDictionary) -> String -> String -> IO ()
setAVDictionary dict key val =
  withCAString key $ \ckey ->
  withCAString val $ \cval ->
  throwAVErrorIf_ "av_dict_set" NoFile $ avDictSet dict ckey cval 0

closeAVIODynBuf :: Ptr AVIOContext -> (CStringLen -> IO a) -> IO a
closeAVIODynBuf c g =
  with nullPtr $ \p ->
    bracket
      (do
        l <- avioCloseDynBuf c p
        b <- peek p
        return (b, fromIntegral l))
      (avFree . fst)
      g

withAVIODynBuf :: ErrorFile -> Ptr (Ptr AVIOContext) -> IO a -> IO (BS.ByteString, a)
withAVIODynBuf ec pb g = do
  throwAVErrorIf_ "avio_open_dyn_buf" ec $
    avioOpenDynBuf pb
  buf <- peek pb
  r <- g `onException`
    closeAVIODynBuf buf return
  b <- closeAVIODynBuf buf BS.packCStringLen
  return (b, r)

withAVFrame :: (Ptr AVFrame -> IO a) -> IO a
withAVFrame =
  bracket
    (throwErrnoIfNull "av_frame_alloc" avFrameAlloc)
    (`with` avFrameFree)

withAVInput :: RawFilePath -> Maybe String -> (Ptr AVFormatContext -> IO a) -> IO a
withAVInput f fmt g =
  BS.useAsCString f $ \cf ->
  with nullPtr $ \a ->
  bracket_
    (do
      avfmt <- maybe (return nullPtr) (`withCAString` avFindInputFormat) fmt
      throwAVErrorIf "avformat_open_input" (FileName f) $
        avformatOpenInput a cf avfmt nullPtr)
    (avformatCloseInput a)
    (g =<< peek a)

findAVStreamInfo :: Ptr AVFormatContext -> IO ()
findAVStreamInfo a = throwAVErrorIf_ "avformat_find_stream_info" (FileContext a) $
  avformatFindStreamInfo a nullPtr

withAVOutput :: Maybe RawFilePath -> String -> (Ptr AVFormatContext -> IO a) -> IO (Maybe BS.ByteString, a)
withAVOutput f fmt g =
  maybe ($ nullPtr) BS.useAsCString f $ \cf ->
  withCAString fmt $ \cfmt ->
  with nullPtr $ \a ->
    bracket_
      (throwAVErrorIf_ "avformat_alloc_output_context2" (maybe NoFile FileName f) $
        avformatAllocOutputContext2 a nullPtr cfmt cf)
      (avformatFreeContext =<< peek a)
      (do
        c <- peek a
        case f of
          Just _ -> (,) Nothing <$> g c
          Nothing -> first Just <$>
            withAVIODynBuf (FileContext c) (#{ptr AVFormatContext, pb} c)
              (g c))

withAVCodec :: Ptr AVFormatContext -> Ptr AVStream -> Ptr AVCodec -> Ptr (Ptr AVDictionary) -> IO a -> IO a
withAVCodec a s c o =
  bracket_
    (do
      sc <- #{peek AVStream, codec} s
      throwAVErrorIf_ "avcodec_open2" (FileContext a) $
        avcodecOpen2 sc c o)
    (do
      sc <- #{peek AVStream, codec} s
      avcodecClose sc)

withAVPacket :: (Ptr AVPacket -> IO a) -> IO a
withAVPacket f = allocaBytes #{size AVPacket} $ \p -> do
  bracket_
    (avInitPacket p)
    (avFreePacket p)
    (do
      #{poke AVPacket, data} p nullPtr
      #{poke AVPacket, size} p (0 :: CInt)
      f p)


avLockmgr :: AVLockmgr ()
avLockmgr p o
  | o == #{const AV_LOCK_CREATE} = (<$) 0 $
    poke p . castStablePtrToPtr =<< newStablePtr =<< newMVar ()
  | o == #{const AV_LOCK_OBTAIN} = (<$) 0 $
    takeMVar =<< deRefStablePtr =<< s
  | o == #{const AV_LOCK_RELEASE} = (<$) 0 $
    (`putMVar` ()) =<< deRefStablePtr =<< s
  | o == #{const AV_LOCK_DESTROY} = (<$) 0 $
    freeStablePtr =<< s
  | otherwise = return (-1)
  where
  s = castPtrToStablePtr <$> peek p
  s :: IO (StablePtr (MVar ()))

data AV = AV 

initAV :: IO AV
initAV = do
  avRegisterAll
  mgr <- mkAVLockmgr avLockmgr
  throwAVErrorIf_ "av_lockmgr_register" NoFile $ avLockmgrRegister mgr
  -- leak mgr
  return AV

data AVMediaType
  = AVMediaTypeUnknown
  | AVMediaTypeVideo
  | AVMediaTypeAudio
  | AVMediaTypeData
  | AVMediaTypeSubtitle
  | AVMediaTypeAttachment
  deriving (Eq, Enum, Bounded, Show)

instance Storable AVMediaType where
  sizeOf _ = sizeOf (undefined :: #type enum AVMediaType)
  alignment _ = alignment (undefined :: #type enum AVMediaType)
  peek p = do
    v <- peek (castPtr p :: Ptr #{type enum AVMediaType})
    return $ case v of 
      #{const AVMEDIA_TYPE_VIDEO}      -> AVMediaTypeVideo
      #{const AVMEDIA_TYPE_AUDIO}      -> AVMediaTypeAudio
      #{const AVMEDIA_TYPE_DATA}       -> AVMediaTypeData
      #{const AVMEDIA_TYPE_SUBTITLE}   -> AVMediaTypeSubtitle
      #{const AVMEDIA_TYPE_ATTACHMENT} -> AVMediaTypeAttachment
      _ -> AVMediaTypeUnknown
  poke p v = poke (castPtr p :: Ptr #{type enum AVMediaType}) $ case v of
    AVMediaTypeUnknown    -> #{const AVMEDIA_TYPE_UNKNOWN}
    AVMediaTypeVideo      -> #{const AVMEDIA_TYPE_VIDEO}
    AVMediaTypeAudio      -> #{const AVMEDIA_TYPE_AUDIO}
    AVMediaTypeData       -> #{const AVMEDIA_TYPE_DATA}
    AVMediaTypeSubtitle   -> #{const AVMEDIA_TYPE_SUBTITLE}
    AVMediaTypeAttachment -> #{const AVMEDIA_TYPE_ATTACHMENT}

data AVProbe = AVProbe
  { avProbeFormat :: BS.ByteString
  , avProbeDuration :: DiffTime
  , avProbeStreams :: [(AVMediaType, BS.ByteString)]
  , avProbeDate :: Maybe ZonedTime
  } deriving (Show)

avProbeHas :: AVMediaType -> AVProbe -> Bool
avProbeHas t = any ((t ==) . fst) . avProbeStreams

avProbeLength :: AVProbe -> Maybe Offset
avProbeLength AVProbe{ avProbeDuration = o } = o > 0 ?> diffTimeOffset o

avTime :: Int64 -> DiffTime
avTime t = realToFrac $ t % #{const AV_TIME_BASE}

avProbe :: RawFilePath -> AV -> IO AVProbe
avProbe f AV = withAVInput f Nothing $ \ic -> do
  findAVStreamInfo ic
  meta <- #{peek AVFormatContext, metadata} ic
  AVProbe
    <$> (BS.packCString =<< #{peek AVInputFormat, name} =<< #{peek AVFormatContext, iformat} ic)
    <*> (avTime <$> #{peek AVFormatContext, duration} ic)
    <*> do
      nb :: CUInt <- #{peek AVFormatContext, nb_streams} ic
      ss <- #{peek AVFormatContext, streams} ic
      forM [0..pred (fromIntegral nb)] $ \i -> do
        c <- #{peek AVStream, codec} =<< peekElemOff ss i
        t <- #{peek AVCodecContext, codec_type} c
        n <- BS.packCString =<< avcodecGetName =<< #{peek AVCodecContext, codec_id} c
        return (t, n)
    <*> ((=<<) (\t -> parseTimeM True defaultTimeLocale "%F %T%Z" t <|> parseTimeM True defaultTimeLocale "%F %T %Z" t) <$>
      ((`orElseM` getAVDictionary meta "creation_time") =<< getAVDictionary meta "date"))


avSeekStream :: Ptr AVFormatContext -> Ptr AVStream -> Ptr AVFrame -> Maybe DiffTime -> IO ()
avSeekStream ctx s frame offset = do
  off <- forM offset $ \o -> do
    den :: CInt <- #{peek AVStream, time_base.den} s
    num :: CInt <- #{peek AVStream, time_base.num} s
    let off = floor $ o * (fromIntegral den) / (fromIntegral num)
    throwAVErrorIf_ "avformat_seek_file" (FileContext ctx) $
      avformatSeekFile ctx 0 (#const INT64_MIN) off off 0
    return off
  si :: CInt <- #{peek AVStream, index} s
  codec <- #{peek AVStream, codec} s
  let
    seek = withAVPacket $ \pkt -> do
      avFrameUnref frame
      throwAVErrorIf_ "av_read_frame" (FileContext ctx) $
        avReadFrame ctx pkt
      psi <- #{peek AVPacket, stream_index} pkt
      if psi /= si then seek else with 0 $ \gpp -> do
        throwAVErrorIf_ "avcodec_decode_video2" (FileContext ctx) $
          avcodecDecodeVideo2 codec frame gpp pkt
        gp <- peek gpp
        if gp == 0 then seek else do
          pts <- avFrameGetBestEffortTimestamp frame
          if any (pts <) off then seek else
            #{poke AVFrame, pts} frame pts -- done
  seek


foreign import ccall unsafe "av.h avFrame_initialize_stream"
  avFrameInitializeStream :: Ptr AVStream -> Ptr AVFormatContext -> Ptr AVStream -> Ptr AVFrame -> CInt -> CInt -> IO ()

foreign import ccall "av.h avFrame_rescale"
  avFrameRescale :: Ptr AVCodecContext -> Ptr AVFrame -> IO CInt

avFrame :: RawFilePath -> Maybe DiffTime -> Maybe Word16 -> Maybe Word16 -> Maybe RawFilePath -> AV -> IO (Maybe BS.ByteString)
avFrame infile offset width height outfile AV =
  withAVInput infile (isimg ?> "image2") $ \inctx ->
  with nullPtr $ \icodecp ->
  withAVDictionary $ \opts -> do
  when isimg $
    #{poke AVFormatContext, video_codec_id} inctx (#{const AV_CODEC_ID_MJPEG} :: AVCodecID)
  findAVStreamInfo inctx

  si <- throwAVErrorIf "av_find_best_stream" (FileContext inctx) $
    avFindBestStream inctx #{const AVMEDIA_TYPE_VIDEO} (-1) (-1) icodecp 0
  nb :: CUInt <- #{peek AVFormatContext, nb_streams} inctx
  isl <- #{peek AVFormatContext, streams} inctx
  forM_ [0..pred (fromIntegral nb)] $ \i ->
    when (i /= si) $ do
      is <- peekElemOff isl (fromIntegral i)
      #{poke AVStream, discard} is (#{const AVDISCARD_ALL} :: #{type enum AVDiscard})
  is <- peekElemOff isl (fromIntegral si)

  setAVDictionary opts "threads" "1"
  icodec <- peek icodecp
  withAVCodec inctx is icodec opts $ withAVFrame $ \frame -> do
    avSeekStream inctx is frame offset
    ffmt <- #{peek AVFrame, format} frame
    fwidth :: CInt <- #{peek AVFrame, width} frame
    fheight :: CInt <- #{peek AVFrame, height} frame
    fmap fst $ withAVOutput outfile (maybe "image2pipe" (const "image2") outfile) $ \outctx -> do
      ocodec <- throwAVErrorIfNull "avcodec_find_encoder(AV_CODEC_ID_MJPEG)" (FileContext outctx) $
        avcodecFindEncoder #{const AV_CODEC_ID_MJPEG}
      os <- throwAVErrorIfNull "avformat_new_stream" (FileContext outctx) $
        avformatNewStream outctx ocodec
      oc :: Ptr AVCodecContext <- #{peek AVStream, codec} os
      avFrameInitializeStream os inctx is frame (maybe (-1) fromIntegral width) (maybe (-1) fromIntegral height)
      owidth <- #{peek AVCodecContext, width} oc
      oheight <- #{peek AVCodecContext, height} oc
      fmts <- #{peek AVCodec, pix_fmts} ocodec
      fmt <- throwAVErrorIf "avcodec_find_best_pix_fmt_of_list" (FileContext outctx) $
        avcodecFindBestPixFmtOfList fmts ffmt 0 nullPtr
      #{poke AVCodecContext, pix_fmt} oc fmt
      when (fmt /= ffmt || owidth /= fwidth || oheight /= fheight) $
        throwAVErrorIf_ "av_frame_get_buffer" (FileContext outctx) $
          avFrameRescale oc frame

      setAVDictionary opts "threads" "1"
      withAVCodec outctx os ocodec opts $
        withAVPacket $ \pkt -> with 0 $ \gpp -> do
          throwAVErrorIf_ "avformat_write_header" (FileContext outctx) $
            avformatWriteHeader outctx nullPtr
          throwAVErrorIf_ "avcodec_encode_video2" (FileContext outctx) $
            avcodecEncodeVideo2 oc pkt frame gpp
          gp <- peek gpp
          when (gp == 0) $
            throwAVError 0 "avcodec_encode_video2 packet" (FileContext outctx)
          throwAVErrorIf_ "av_write_frame" (FileContext outctx) $
            avWriteFrame outctx pkt
          throwAVErrorIf_ "av_write_trailer" (FileContext outctx) $
            avWriteTrailer outctx
  where
  isimg = isNothing offset
