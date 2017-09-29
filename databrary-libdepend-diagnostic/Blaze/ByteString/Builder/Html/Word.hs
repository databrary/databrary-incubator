{- utility functions for creating escaped strings to be consumed
   blaze html (the templating system)
-}

{-# LANGUAGE OverloadedStrings #-}
module Blaze.ByteString.Builder.Html.Word
  ( wordHtmlEscaped
  , fromHtmlEscapedWord
  , fromHtmlEscapedWordList
  , fromHtmlEscapedByteString
  , fromHtmlEscapedLazyByteString
  , fromHtmlEscapedText
  , fromHtmlEscapedLazyText
  ) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Builder.Prim as P
import Data.ByteString.Internal (c2w)
import qualified Data.ByteString.Lazy as BSL
import Data.Word (Word8)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE

wordHtmlEscaped :: P.BoundedPrim Word8
wordHtmlEscaped =
  P.condB (>  c2w '>' ) (P.condB (== c2w '\DEL') P.emptyB $ P.liftFixedToBounded P.word8) $
  P.condB (== c2w '<' ) (fixed4 ('&',('l',('t',';')))) $        -- &lt;
  P.condB (== c2w '>' ) (fixed4 ('&',('g',('t',';')))) $        -- &gt;
  P.condB (== c2w '&' ) (fixed5 ('&',('a',('m',('p',';'))))) $  -- &amp;
  P.condB (== c2w '"' ) (fixed6 ('&',('q',('u',('o',('t',';')))))) $  -- &quot;
  P.condB (== c2w '\'') (fixed5 ('&',('#',('3',('9',';'))))) $  -- &#39;
  P.condB (\c -> c >= c2w ' ' || c == c2w '\t' || c == c2w '\n' || c == c2w '\r')
        (P.liftFixedToBounded P.word8) P.emptyB
  where
  fixed4 x = P.liftFixedToBounded $ const x P.>$<
    P.char8 P.>*< P.char8 P.>*< P.char8 P.>*< P.char8
  fixed5 x = P.liftFixedToBounded $ const x P.>$<
    P.char8 P.>*< P.char8 P.>*< P.char8 P.>*< P.char8 P.>*< P.char8
  fixed6 x = P.liftFixedToBounded $ const x P.>$<
    P.char8 P.>*< P.char8 P.>*< P.char8 P.>*< P.char8 P.>*< P.char8 P.>*< P.char8

fromHtmlEscapedWord :: Word8 -> B.Builder
fromHtmlEscapedWord = P.primBounded wordHtmlEscaped

fromHtmlEscapedWordList :: [Word8] -> B.Builder
fromHtmlEscapedWordList = P.primMapListBounded wordHtmlEscaped

fromHtmlEscapedByteString :: BS.ByteString -> B.Builder
fromHtmlEscapedByteString = P.primMapByteStringBounded wordHtmlEscaped

fromHtmlEscapedLazyByteString :: BSL.ByteString -> B.Builder
fromHtmlEscapedLazyByteString = P.primMapLazyByteStringBounded wordHtmlEscaped

fromHtmlEscapedText :: T.Text -> B.Builder
fromHtmlEscapedText = TE.encodeUtf8BuilderEscaped wordHtmlEscaped

fromHtmlEscapedLazyText :: TL.Text -> B.Builder
fromHtmlEscapedLazyText = TLE.encodeUtf8BuilderEscaped wordHtmlEscaped

