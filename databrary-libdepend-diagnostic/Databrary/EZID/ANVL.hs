module Databrary.EZID.ANVL
  ( ANVL
  , encode
  , parse
  ) where

import Control.Applicative ((<|>))
import qualified Data.Attoparsec.ByteString.Char8 as P
import Data.Bits (shiftL, (.|.))
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Builder.Prim as BP
import Data.ByteString.Internal (c2w)
import Data.Char (isHexDigit, digitToInt)
import Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Word (Word8)

type ANVL = [(T.Text, T.Text)]

charEscaped :: Bool -> BP.BoundedPrim Word8
charEscaped colon =
  BP.condB (\c -> c == c2w '%' || (colon && c == c2w ':') || c < c2w ' ')
    (BP.liftFixedToBounded $ (,) '%' BP.>$< BP.char8 BP.>*< BP.word8HexFixed)
    (BP.liftFixedToBounded BP.word8)

encode :: ANVL -> B.Builder
encode = foldMap $ \(n,v) ->
  TE.encodeUtf8BuilderEscaped (charEscaped True) n <> B.char8 ':' <> B.char8 ' ' <> TE.encodeUtf8BuilderEscaped (charEscaped False) v <> B.char8 '\n'

parse :: P.Parser ANVL
parse = P.sepBy nv P.endOfLine <* P.skipSpace
  where
  hd = digitToInt <$> P.satisfy isHexDigit
  pe = P.char '%' >> (.|.) . (`shiftL` 4) <$> hd <*> hd
  textWhile1 p = either (fail . show) return . TE.decodeUtf8' =<< P.takeWhile1 p
  tx d = mconcat <$> P.many' (textWhile1 (`notElem` '%':d) <|> (T.singleton . toEnum <$> pe))
  nv = do
    n <- tx ":\n" P.<?> "name"
    _ <- P.char ':'
    P.skipMany (P.char ' ')
    v <- tx "\n" P.<?> "value"
    return (n, v)
