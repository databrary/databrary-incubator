{-# LANGUAGE OverloadedStrings #-}
module Databrary.HTTP.Parse
  ( Content(..)
  , FileContent
  , parseRequestContent
  ) where

import Control.Monad (when, unless)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as JSON
import qualified Data.Attoparsec.ByteString as AP
import qualified Data.ByteString as BS
import Data.IORef (newIORef, readIORef, writeIORef)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TE
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Internal.Lazy as TL (chunk)
import Data.Word (Word64)
import Network.HTTP.Types (requestEntityTooLarge413, unsupportedMediaType415, hContentType)
import Network.Wai
import Network.Wai.Parse
import System.IO (Handle)

import Databrary.Has (peek, peeks)
import Databrary.Action.Types
import Databrary.Store.Temp
import Databrary.HTTP.Request (lookupRequestHeader)
import Databrary.Action.Response (response, emptyResponse, result, unsafeResult)

requestTooLarge :: Response
requestTooLarge = emptyResponse requestEntityTooLarge413 []

type ChunkParser a = IO BS.ByteString -> IO a

_mapChunks :: (a -> b) -> ChunkParser a -> ChunkParser b
_mapChunks f parse next = f <$> parse next

_nullChunks :: ChunkParser Word64
_nullChunks next = go 0 where
  go n = do
    b <- next
    if BS.null b
      then return n
      else go (n + fromIntegral (BS.length b))

limitChunks :: Word64 -> ChunkParser a -> ChunkParser a
limitChunks lim parse next = do
  len <- liftIO $ newIORef 0
  parse $ do
    n <- readIORef len
    b <- next
    let n' = n + fromIntegral (BS.length b)
    when (n' > lim) $ result requestTooLarge
    writeIORef len n'
    return b

writeChunks :: Handle -> ChunkParser ()
writeChunks h next = run where
  run = do
    b <- next
    unless (BS.null b) $
      BS.hPut h b >> run

parserChunks :: AP.Parser a -> ChunkParser (AP.Result a)
parserChunks parser next = run (AP.parse parser) where
  run p = do
    b <- next
    let r = p b
    if BS.null b
      then return r
      else run $ AP.feed r

textChunks :: TE.OnDecodeError -> ChunkParser TL.Text
textChunks err next = run (TE.streamDecodeUtf8With err) where
  run f = do
    b <- next
    let TE.Some t r f' = f b
    if BS.null b
      then return $ TL.fromStrict $ maybe t (T.snoc t) $ err "textChunks: invalid UTF-8" . Just . fst =<< BS.uncons r
      else TL.chunk t <$> run f'

textChunks' :: ChunkParser TL.Text
textChunks' = textChunks (\e _ -> unsafeResult $ response unsupportedMediaType415 [] e)


_mapBackEnd :: (a -> b) -> BackEnd a -> BackEnd b
_mapBackEnd f back param info next = f <$> back param info next

rejectBackEnd :: BackEnd a
rejectBackEnd _ _ _ = result requestTooLarge


_parseRequestChunks :: ChunkParser a -> ActionM a
_parseRequestChunks p = liftIO . p =<< peeks requestBody

limitRequestChunks :: Word64 -> ChunkParser a -> ActionM a
limitRequestChunks lim p = do
  rq <- peek
  case requestBodyLength rq of
    KnownLength l | l > lim -> result requestTooLarge
    _ -> liftIO $ limitChunks lim p $ requestBody rq

data Content a
  = ContentForm
    { contentFormParams :: [Param]
    , contentFormFiles :: [File a]
    }
  | ContentJSON JSON.Value
  | ContentText TL.Text
  | ContentUnknown

maxTextSize :: Word64
maxTextSize = 1024*1024

class FileContent a where
  parseFileContent :: IO BS.ByteString -> ActionM a

instance FileContent () where
  parseFileContent _ = result requestTooLarge

instance FileContent TempFile where
  parseFileContent = makeTempFile . flip writeChunks

instance FileContent JSON.Value where
  parseFileContent b = liftIO $ either (result . response unsupportedMediaType415 []) return . AP.eitherResult =<< parserChunks JSON.json b

instance FileContent TL.Text where
  parseFileContent = liftIO . textChunks'

parseFormContent :: RequestBodyType -> ActionM (Content a)
parseFormContent t = uncurry ContentForm
  <$> limitRequestChunks maxTextSize (liftIO . sinkRequestBody rejectBackEnd t)

parseFormFileContent :: FileContent a => (FileInfo BS.ByteString -> Word64) -> RequestBodyType -> ActionM (Content a)
parseFormFileContent ff rt = do
  app <- peek
  (p, f) <- liftIO $ do
    let be fn fi fb = case ff fi{ fileContent = fn } of
          0 -> result requestTooLarge
          m -> limitChunks m (\b -> runActionM (parseFileContent b) app) fb
    sinkRequestBody be rt (requestBody $ contextRequest app)
  return $ ContentForm p f

parseJSONContent :: ActionM (Content a)
parseJSONContent = maybe ContentUnknown ContentJSON . AP.maybeResult
  <$> limitRequestChunks maxTextSize (parserChunks JSON.json)

parseTextContent :: ActionM (Content a)
parseTextContent = ContentText <$> limitRequestChunks maxTextSize textChunks'
  -- really would be better to catch the error and return ContentUnknown

parseRequestContent :: FileContent a => (BS.ByteString -> Word64) -> ActionM (Content a)
parseRequestContent ff = do
  ct <- peeks $ lookupRequestHeader hContentType
  case fmap parseContentType ct of
    Just ("application/x-www-form-urlencoded", _) ->
      parseFormContent UrlEncoded
    Just ("multipart/form-data", attrs) | Just bound <- lookup "boundary" attrs ->
      parseFormFileContent (ff . fileContent) $ Multipart bound
    Just ("text/json", _) ->
      parseJSONContent
    Just ("application/json", _) ->
      parseJSONContent
    Just ("text/plain", _) ->
      parseTextContent
    _ -> return ContentUnknown
