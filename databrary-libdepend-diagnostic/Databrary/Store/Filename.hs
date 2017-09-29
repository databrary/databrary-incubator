module Databrary.Store.Filename
  ( makeFilename
  ) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Lazy as BSL
import Data.Char (isSpace)
import Data.List (intersperse)
import Data.Monoid ((<>))
import qualified Data.Text as T

truncateFilename :: T.Text -> Int
truncateFilename s
  | len <= maxlen = len
  | otherwise = fr maxlen where
  len = T.length s
  fr n
    | n < maxlen `div` 2 = maxlen
    | isSpace (T.index s n) = n
    | otherwise = fr $ pred n
  maxlen = 32

buildFilename :: T.Text -> BSB.Builder
buildFilename t = fc (T.unpack t) len True where
  len = truncateFilename t
  fc [] _ _ = mempty
  fc _ 0 _ = mempty
  fc (c:s) n p
    | c < ',' || c `elem` "/?\\" = if p then r True else BSB.char8 '_' <> r True
    | otherwise = BSB.charUtf8 c <> r False
    where r = fc s (pred n)

makeFilename :: [T.Text] -> BS.ByteString
makeFilename = BSL.toStrict . BSB.toLazyByteString . mconcat . intersperse (BSB.char8 '-') . map buildFilename
