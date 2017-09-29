{-# LANGUAGE OverloadedStrings, FunctionalDependencies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Databrary.JSON
  ( module Data.Aeson
  , module Data.Aeson.Types
  , ToObject
  , objectEncoding
  , mapObjects
  , ToNestedObject(..)
  , (.=.)
  , (.=?)
  , (.!)
  , (.!?)
  , Record(..)
  , (.<>)
  , recordObject
  , recordEncoding
  , mapRecords
  , (.=:)
  , recordMap
  , eitherJSON
  , Query
  , jsonQuery
  , escapeByteString
  , quoteByteString
  ) where

import Data.Aeson
import Data.Aeson.Types
import Data.Aeson.Encode (encodeToTextBuilder)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Builder.Prim as BP
import Data.ByteString.Internal (c2w)
import qualified Data.HashMap.Strict as HM
import Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TLB
import qualified Data.Vector as V
import Data.Word (Word8)
import Network.HTTP.Types (Query)
import qualified Text.Blaze.Html as Html
import qualified Text.Blaze.Html.Renderer.Text as Html

newtype UnsafeEncoding = UnsafeEncoding Encoding

instance KeyValue [Pair] where
  k .= v = [k .= v]

instance KeyValue Object where
  k .= v = HM.singleton k $ toJSON v

class (Monoid o, KeyValue o) => ToObject o

instance ToObject Series
instance ToObject [Pair]
instance ToObject Object

objectEncoding :: Series -> Encoding
objectEncoding = pairs

mapObjects :: (Functor t, Foldable t) => (a -> Series) -> t a -> Encoding
mapObjects f = foldable . fmap (UnsafeEncoding . pairs . f)

class (ToObject o, ToJSON u) => ToNestedObject o u | o -> u where
  nestObject :: ToJSON v => T.Text -> ((o -> u) -> v) -> o

instance ToJSON UnsafeEncoding where
  toJSON = error "toJSON UnsafeEncoding"
  toEncoding (UnsafeEncoding e) = e

instance ToNestedObject Series UnsafeEncoding where
  nestObject k f = k .= f (UnsafeEncoding . pairs)

instance ToNestedObject [Pair] Value where
  nestObject k f = k .= f object

instance ToNestedObject Object Value where
  nestObject k f = k .= f Object

infixr 8 .=.
(.=.) :: ToNestedObject o u => T.Text -> o -> o
k .=. v = nestObject k (\f -> f v)

infixr 8 .=?
(.=?) :: (ToObject o, ToJSON v) => T.Text -> Maybe v -> o
_ .=? Nothing = mempty
k .=? (Just v) = k .= v

data Record k o = Record
  { recordKey :: !k
  , _recordObject :: o
  }

infixl 5 .<>
(.<>) :: Monoid o => Record k o -> o -> Record k o
Record k o .<> a = Record k $ o <> a

recordObject :: (ToJSON k, ToObject o) => Record k o -> o
recordObject (Record k o) = "id" .= k <> o

recordEncoding :: ToJSON k => Record k Series -> Encoding
recordEncoding = objectEncoding . recordObject

mapRecords :: (Functor t, Foldable t, ToJSON k) => (a -> Record k Series) -> t a -> Encoding
mapRecords = mapObjects . (recordObject .)

infixr 8 .=:
(.=:) :: (ToJSON k, ToNestedObject o u) => T.Text -> Record k o -> o
(.=:) k = (.=.) k . recordObject

recordMap :: (ToJSON k, ToNestedObject o u) => [Record k o] -> o
recordMap = foldMap (\r -> tt (toJSON $ recordKey r) .=. recordObject r) where
  tt (String t) = t
  tt v = TL.toStrict $ TLB.toLazyText $ encodeToTextBuilder v

(.!) :: FromJSON a => Array -> Int -> Parser a
a .! i = maybe (fail $ "index " ++ show i ++ " out of range") parseJSON $ a V.!? i

(.!?) :: FromJSON a => Array -> Int -> Parser (Maybe a)
a .!? i = mapM parseJSON $ a V.!? i

resultToEither :: Result a -> Either String a
resultToEither (Error e) = Left e
resultToEither (Success a) = Right a

eitherJSON :: FromJSON a => Value -> Either String a
eitherJSON = resultToEither . fromJSON

instance ToJSON BS.ByteString where
  toJSON = String . TE.decodeUtf8 -- questionable

instance FromJSON BS.ByteString where
  parseJSON = fmap TE.encodeUtf8 . parseJSON

instance ToJSON Html.Html where
  toJSON = toJSON . Html.renderHtml
  toEncoding = toEncoding . Html.renderHtml

jsonQuery :: Monad m => (BS.ByteString -> Maybe BS.ByteString -> m (Maybe Encoding)) -> Query -> m Series
jsonQuery _ [] = return mempty
jsonQuery f ((k,v):q) = do
  o <- f k v
  maybe id ((<>) . (TE.decodeLatin1 k .=) . UnsafeEncoding) o <$> jsonQuery f q

wordEscaped :: Char -> BP.BoundedPrim Word8
wordEscaped q =
  BP.condB (== c2w q) (backslash q) $
  BP.condB (== c2w '\\') (backslash '\\') $
  BP.condB (>= c2w ' ') (BP.liftFixedToBounded BP.word8) $
  BP.condB (== c2w '\n') (backslash 'n') $
  BP.condB (== c2w '\r') (backslash 'r') $
  BP.condB (== c2w '\t') (backslash 't') $
    BP.liftFixedToBounded $ (\c -> ('\\', ('u', fromIntegral c))) BP.>$< BP.char8 BP.>*< BP.char8 BP.>*< BP.word16HexFixed
  where
  backslash c = BP.liftFixedToBounded $ const ('\\', c) BP.>$< BP.char8 BP.>*< BP.char8

-- | Escape (but do not quote) a ByteString
escapeByteString :: Char -> BS.ByteString -> B.Builder
escapeByteString = BP.primMapByteStringBounded . wordEscaped

quoteByteString :: Char -> BS.ByteString -> B.Builder
quoteByteString q s = B.char8 q <> escapeByteString q s <> B.char8 q
