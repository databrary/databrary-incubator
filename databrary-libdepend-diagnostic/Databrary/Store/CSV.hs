module Databrary.Store.CSV
  ( buildCSV
  ) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Builder.Prim as BP
import qualified Data.ByteString.Char8 as BSC
import Data.ByteString.Internal (c2w)
import Data.Monoid ((<>))

inter :: B.Builder -> [B.Builder] -> B.Builder
inter _ [] = mempty
inter d (x:l) = x <> mconcat (map (d <>) l)

csvCell :: BS.ByteString -> B.Builder
csvCell t
  | BSC.any (`elem` "\",\r\n") t = q <> BP.primMapByteStringBounded quote t <> q
  | otherwise = B.byteString t
  where
  qw = c2w '"'
  q = B.word8 qw
  quote = BP.condB (== qw)
    (BP.liftFixedToBounded $ const (qw, qw) BP.>$< BP.word8 BP.>*< BP.word8)
    (BP.liftFixedToBounded BP.word8)

csvRow :: [BS.ByteString] -> B.Builder
csvRow r = inter (B.char8 ',') (map csvCell r) <> B.char8 '\n'

buildCSV :: [[BS.ByteString]] -> B.Builder
buildCSV = mconcat . map csvRow
