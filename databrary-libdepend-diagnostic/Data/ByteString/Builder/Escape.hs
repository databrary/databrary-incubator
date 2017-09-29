{- utility functions for creating escaped strings to be consumed
   blaze html (the templating system)
-}
module Data.ByteString.Builder.Escape where

import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import Data.ByteString.Builder (Builder)
import Data.ByteString.Builder.Prim
import Data.ByteString.Internal (c2w)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8BuilderEscaped)
import Data.Word (Word8)

word8EscapedWith :: Word8 -> (Word8 -> Bool) -> BoundedPrim Word8
word8EscapedWith e c = condB c (liftFixedToBounded $ (,) e >$< word8 >*< word8) (liftFixedToBounded word8)

char8EscapedWith :: Char -> [Char] -> BoundedPrim Word8
char8EscapedWith e c = word8EscapedWith (c2w e) (`elem` w) where w = map c2w c

escapeByteStringWith :: Word8 -> (Word8 -> Bool) -> S.ByteString -> Builder
escapeByteStringWith e c = primMapByteStringBounded (word8EscapedWith e c)

escapeLazyByteStringWith :: Word8 -> (Word8 -> Bool) -> L.ByteString -> Builder
escapeLazyByteStringWith e c = primMapLazyByteStringBounded (word8EscapedWith e c)

escapeByteStringCharsWith :: Char -> [Char] -> S.ByteString -> Builder
escapeByteStringCharsWith e c = primMapByteStringBounded (char8EscapedWith e c)

escapeLazyByteStringCharsWith :: Char -> [Char] -> L.ByteString -> Builder
escapeLazyByteStringCharsWith e c = primMapLazyByteStringBounded (char8EscapedWith e c)

escapeTextWith :: Char -> [Char] -> Text -> Builder
escapeTextWith e c = encodeUtf8BuilderEscaped (char8EscapedWith e c)
