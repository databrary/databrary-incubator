{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving, ScopedTypeVariables, TypeSynonymInstances, FlexibleInstances, RecordWildCards #-}
module Databrary.Store.Config
  ( Path(..)
  , pathKey
  , keyPath
  , Value(..)
  , ConfigMap
  , Config
  , configMap
  , configPath
  , load
  , Configurable(..)
  , (!)
  ) where

import Prelude hiding (lookup)

import Control.Applicative ((<|>))
import Control.Arrow (first, (***))
import Control.Exception (Exception, throw)
import Control.Monad ((<=<))
import qualified Data.Aeson.Types as JSON
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.Foldable (fold)
import qualified Data.HashMap.Strict as HM
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.String (IsString(..))
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Typeable (Typeable, TypeRep, typeRep)
import qualified Data.Vector as V
import qualified Text.Parsec as P
import qualified Text.Parsec.ByteString.Lazy as P
import qualified Text.Parsec.Token as PT

import Databrary.Ops

type Key = BS.ByteString
newtype Path = Path { pathList :: [Key] } deriving (Monoid)

pathKey :: Path -> Key
pathKey (Path p) = BS.intercalate (BSC.singleton '.') p

keyPath :: Key -> Path
keyPath = Path . BSC.split '.'

pathSnoc :: Path -> Key -> Path
pathSnoc (Path l) k = Path (l ++ [k])

instance Show Path where
  showsPrec p = showsPrec p . pathKey

instance IsString Path where
  fromString = keyPath . fromString

data ConfigError
  = ParseError P.ParseError
  | ConflictError
    { errorPath :: Path
    , errorValue1, errorValue2 :: Value
    }
  | ValueError
    { errorPath :: Path
    , errorValue :: Value
    , errorNeeded :: TypeRep
    }
  deriving (Typeable, Show)

instance Exception ConfigError

data Value
  = Empty
  | Boolean !Bool
  | Integer !Integer
  | String !BS.ByteString
  | List [Value]
  | Sub !ConfigMap
  deriving (Typeable, Eq, Show)

type ConfigMap = HM.HashMap BS.ByteString Value

data Config = Config
  { configPath :: !Path
  , configMap :: !ConfigMap
  } deriving (Typeable, Show)

topConfig :: ConfigMap -> Config
topConfig = Config (Path [])

unionValue :: Path -> Value -> Value -> Value
unionValue _ Empty v = v
unionValue _ v Empty = v
unionValue p (Sub m1) (Sub m2) = Sub $ unionConfig p m1 m2
unionValue p v1 v2
  | v1 == v2 = v1
  | otherwise = throw $ ConflictError{ errorPath = p, errorValue1 = v1, errorValue2 = v2 }

unionConfig :: Path -> ConfigMap -> ConfigMap -> ConfigMap
unionConfig p = HM.foldrWithKey $ \k -> HM.insertWith (flip $ unionValue (pathSnoc p k)) k

-- |Merge two configs, throwing 'ConflictError' on conflicts
instance Monoid Config where
  mempty = topConfig HM.empty
  Config (Path p1) m1 `mappend` Config (Path p2) m2 = Config p m where
    (p', (p1', p2')) = cpfx p1 p2
    p = Path p'
    m = unionConfig p (nest m1 p1') (nest m2 p2')
    cpfx (a:al) (b:bl) | a == b = first (a :) $ cpfx al bl
    cpfx al bl = ([], (al, bl))
    nest = foldr (\k -> HM.singleton k . Sub)

lookup :: Path -> ConfigMap -> Value
lookup (Path []) m = Sub m
lookup (Path [k]) m | Just v <- HM.lookup k m = v
lookup (Path (k:l)) m | Just (Sub km) <- HM.lookup k m = lookup (Path l) km
lookup _ _ = Empty

parser :: P.Parser ConfigMap
parser = whiteSpace *> block mempty HM.empty <* P.eof where
  block p m = (block p =<< pair p m) <|> return m
  pair p m = do
    ks <- identifier P.<?> "key"
    let k = BSC.pack ks
        kp = pathSnoc p k
    km <- case HM.lookupDefault Empty k m of
      Empty -> return Nothing
      Sub km -> return $ Just km
      _ -> fail $ "Duplicate key value: " ++ show kp
    kv <- lexeme dot *> (Sub <$> pair kp (fold km)) <|> rhs kp km
    return $ HM.insert k kv m
  rhs p Nothing = sub p HM.empty <|>
    lexeme (P.char '=') *> val
  rhs p (Just m) = sub p m
  sub p m = Sub <$> braces (block p m)
  val = P.choice
    [ Boolean True <$ reserved "true"
    , Boolean False <$ reserved "false"
    , Integer <$> integer
    , String . BSC.pack <$> stringLiteral
    , List <$> brackets (commaSep val)
    ] P.<?> "value"
  PT.TokenParser{..} = PT.makeTokenParser PT.LanguageDef
    { PT.commentStart = ""
    , PT.commentEnd = ""
    , PT.commentLine = "#"
    , PT.nestedComments = False
    , PT.identStart = P.letter
    , PT.identLetter = (P.alphaNum <|> P.oneOf "-_")
    , PT.opStart = P.unexpected "operator"
    , PT.opLetter = P.unexpected "operator"
    , PT.reservedNames = []
    , PT.reservedOpNames = ["="]
    , PT.caseSensitive = True
    }

load :: FilePath -> IO Config
load f = either (throw . ParseError) (return . topConfig) =<< P.parseFromFile parser f

class Typeable a => Configurable a where
  get :: Path -> Config -> a
  get p (Config cp m) = fromMaybe (throw ValueError{ errorPath = cp <> p, errorValue = v, errorNeeded = typeRep r}) r where
    v = lookup p m
    r = config v
  config :: Value -> Maybe a

instance Configurable Value where
  get p (Config _ m) = lookup p m
  config = Just

instance Configurable ConfigMap where
  config (Sub m) = Just m
  config Empty = Just HM.empty
  config _ = Nothing

instance Configurable Config where
  get p c = Config (configPath c <> p) $ get p c
  config v = topConfig <$> config v

instance Configurable a => Configurable (Maybe a) where
  config Empty = Just Nothing
  config v = Just <$> config v

instance Configurable Bool where
  config (Boolean b) = Just b
  config _ = Nothing

instance Configurable Integer where
  config (Integer i) = Just i
  config _ = Nothing

instance Configurable BS.ByteString where
  config (String s) = Just s
  config _ = Nothing

instance {-# OVERLAPPABLE #-} Configurable a => Configurable [a] where
  config (List l) = mapM config l
  config _ = Nothing

instance Configurable T.Text where
  config = rightJust . TE.decodeUtf8' <=< config

instance {-# OVERLAPPING #-} Configurable String where
  config v = BSC.unpack <$> config v

configBoundedInt :: forall a . (Integral a, Bounded a) => Value -> Maybe a
configBoundedInt = f <=< config where
  f i = i >= toInteger (minBound :: a) && i <= toInteger (maxBound :: a) ?> fromInteger i

instance Configurable Int where
  config = configBoundedInt

infixl 9 !
(!) :: Configurable a => Config -> Path -> a
(!) = flip get

instance JSON.ToJSON Config where
  toJSON = JSON.toJSON . configMap
  toEncoding = JSON.toEncoding . configMap

instance JSON.ToJSON ConfigMap where
  toJSON = JSON.object . map (TE.decodeUtf8 *** JSON.toJSON) . HM.toList
  toEncoding = JSON.pairs . HM.foldrWithKey (\k v -> (TE.decodeUtf8 k JSON..= v <>)) mempty

instance JSON.ToJSON Value where
  toJSON Empty = JSON.Null
  toJSON (Boolean b) = JSON.Bool b
  toJSON (String s) = JSON.String $ TE.decodeUtf8 s
  toJSON (Integer i) = JSON.Number $ fromInteger i
  toJSON (List l) = JSON.Array $ V.fromList $ map JSON.toJSON l
  toJSON (Sub c) = JSON.toJSON c
  toEncoding (List l) = JSON.foldable l
  toEncoding (Sub c) = JSON.toEncoding c
  toEncoding v = JSON.toEncoding $ JSON.toJSON v
