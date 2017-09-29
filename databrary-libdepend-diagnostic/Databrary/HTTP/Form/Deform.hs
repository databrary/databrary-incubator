{-# LANGUAGE OverloadedStrings, TypeFamilies #-}
module Databrary.HTTP.Form.Deform
  ( DeformT
  , runDeform
  , deformSync'
  , (.:>)
  , withSubDeforms
  , deformOptional
  , deformNonEmpty
  , Deform(..)
  , deformError
  , deformError'
  , deformMaybe'
  , deformGuard
  , deformCheck
  , deformParse
  , deformRead
  , deformRequired
  , textInteger
  ) where

import Control.Applicative (Alternative(..), liftA2)
import Control.Arrow (first, second, (***), left)
import Control.Monad (MonadPlus(..), liftM, mapAndUnzipM, guard)
import Control.Monad.Reader (MonadReader(..), asks)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Trans.Control (MonadTransControl(..))
import Control.Monad.Writer.Class (MonadWriter(..))
import qualified Data.Aeson as JSON
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.UTF8 as BSU
import qualified Data.HashMap.Strict as HM
import Data.Int (Int64, Int32, Int16)
import Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Read as TR
import Data.Time (fromGregorian)
import Data.Time.Format (parseTimeM, defaultTimeLocale)
import qualified Data.Vector as V
import qualified Database.PostgreSQL.Typed.Range as Range (Range(Empty))
import qualified Network.URI as URI
import Network.Wai.Parse (FileInfo)
import Text.Read (readEither)

import Databrary.Ops
import Databrary.Model.URL
import Databrary.Model.Time
import Databrary.Model.Offset
import Databrary.Model.Segment
import Databrary.HTTP.Form
import Databrary.HTTP.Form.Errors

newtype DeformT f m a = DeformT { runDeformT :: Form f -> m (FormErrors, Maybe a) }

instance MonadTrans (DeformT f) where
  lift m = DeformT $ \_ ->
    liftM ((,) mempty . Just) m

instance MonadTransControl (DeformT f) where
  type StT (DeformT f) a = (FormErrors, Maybe a)
  liftWith f = DeformT $ \d ->
    liftM ((,) mempty . Just) $ f $ \t -> runDeformT t d
  restoreT m = DeformT $ \_ -> m

instance MonadIO m => MonadIO (DeformT f m) where
  liftIO = lift . liftIO

instance Functor m => Functor (DeformT f m) where
  fmap f (DeformT m) = DeformT $ \d ->
    second (fmap f) `fmap` m d

instance Applicative m => Applicative (DeformT f m) where
  pure a = DeformT $ \_ -> pure (mempty, Just a)
  DeformT f <*> DeformT v = DeformT $ \d ->
    liftA2 k (f d) (v d) where
    k (ef, mf) (ev, mv) = (ef <> ev, mf <*> mv)

instance Monad m => Monad (DeformT f m) where
  return = lift . return
  DeformT x >>= f = DeformT $ \d -> do
    (ex, mx) <- x d
    case mx of
      Nothing -> return (ex, Nothing)
      Just vx -> first (ex <>) `liftM` runDeformT (f $! vx) d
  fail = deformError' . T.pack

instance Monad m => MonadPlus (DeformT f m) where
  mzero = DeformT $ \_ -> return (mempty, Nothing)
  DeformT a `mplus` DeformT b = DeformT $ \d -> do
    ar <- a d
    case ar of
      (er, Just _) | nullFormErrors er -> return ar
      _ -> b d

instance (Applicative m, Monad m) => Alternative (DeformT f m) where
  empty = mzero
  (<|>) = mplus

instance Monad m => MonadReader (Form f) (DeformT f m) where
  ask = DeformT $ \d -> return (mempty, Just d)
  reader f = DeformT $ \d -> return (mempty, Just (f d))
  local f (DeformT a) = DeformT $ a . f

instance Monad m => MonadWriter FormErrors (DeformT f m) where
  writer (a, e) = DeformT $ \_ -> return (e, Just a)
  listen (DeformT a) = DeformT $ \d -> do
    (e, r) <- a d
    return (e, fmap (flip (,) e) r)
  pass (DeformT a) = DeformT $ \q -> do
    (e, mrf) <- a q
    case mrf of
      Just (r, f) -> return (f e, Just r)
      Nothing -> return (e, Nothing)

runDeform :: Functor m => DeformT f m a -> FormData f -> m (Either FormErrors a)
runDeform (DeformT fa) = fmap fr . fa . initForm where
  fr (e, Just a) | nullFormErrors e = Right a
  fr (e, _) = Left e

deformSync' :: Functor m => DeformT f m a -> DeformT f m a
deformSync' (DeformT f) = DeformT $ fmap sync . f where
  sync (e, a) = (e, guard (nullFormErrors e) >> a)

withSubDeform :: (Functor m, Monad m) => FormKey -> DeformT f m a -> DeformT f m a
withSubDeform k (DeformT a) = DeformT $ fmap (first (unsubFormErrors k)) . a . subForm k

infixr 2 .:>
(.:>) :: (Functor m, Monad m) => T.Text -> DeformT f m a -> DeformT f m a
(.:>) = withSubDeform . FormField

withSubDeforms :: (Functor m, Monad m) => (FormKey -> DeformT f m a) -> DeformT f m [a]
withSubDeforms s = DeformT $
  fmap (unsubFormsErrors *** sequence) . mapAndUnzipM (uncurry $ runDeformT . s) . subForms

deformErrorWith :: Monad m => Maybe a -> FormErrorMessage -> DeformT f m a
deformErrorWith r e = DeformT $ \_ -> return (singletonFormError e, r)

deformError :: Monad m => FormErrorMessage -> DeformT f m ()
deformError = deformErrorWith (Just ())

deformError' :: Monad m => FormErrorMessage -> DeformT f m a
deformError' = deformErrorWith Nothing

deformMaybe' :: Monad m => FormErrorMessage -> Maybe a -> DeformT f m a
deformMaybe' e = maybe (deformError' e) return

deformEither :: (Functor m, Monad m) => a -> Either FormErrorMessage a -> DeformT f m a
deformEither def = either ((<$) def . deformError) return

deformGuard :: (Monad m) => FormErrorMessage -> Bool -> DeformT f m ()
deformGuard _ True = return ()
deformGuard e False = deformError e

deformCheck :: (Functor m, Monad m) => FormErrorMessage -> (a -> Bool) -> a -> DeformT f m a
deformCheck e f = (>$) (deformGuard e . f)

deformOptional :: (Functor m, Monad m) => DeformT f m a -> DeformT f m (Maybe a)
deformOptional f = opt =<< asks formDatum where
  opt FormDatumNone = return Nothing
  opt _ = Just <$> f

deformNonEmpty :: (Functor m, Monad m) => DeformT f m a -> DeformT f m (Maybe a)
deformNonEmpty f = opt =<< asks formDatum where
  opt FormDatumNone = return Nothing
  opt (FormDatumBS s) | BS.null s = return Nothing
  opt (FormDatumJSON (JSON.String s)) | T.null s = return Nothing
  opt (FormDatumJSON (JSON.Object o)) | HM.null o = return Nothing
  opt (FormDatumJSON (JSON.Array v)) | V.null v = return Nothing
  opt (FormDatumJSON JSON.Null) = return Nothing
  opt _ = Just <$> f

deformParse :: (Functor m, Monad m) => a -> (FormDatum -> Either FormErrorMessage a) -> DeformT f m a
deformParse def p = deformEither def =<< asks (p . formDatum)

deformParseJSON :: (Functor m, Monad m, JSON.FromJSON a) => a -> (Maybe BS.ByteString -> Either FormErrorMessage a) -> DeformT f m a
deformParseJSON def p = do
  d <- asks formDatum
  case d of
    FormDatumNone -> deformEither def $ p Nothing
    FormDatumBS b -> deformEither def $ p $ Just b
    FormDatumJSON j -> case JSON.fromJSON j of
      JSON.Error e -> def <$ deformError (T.pack e)
      JSON.Success r -> return r
    FormDatumFlag -> deformEither def $ p Nothing

class Deform f a where
  deform :: (Functor m, Monad m) => DeformT f m a

instance Deform f FormDatum where
  deform = asks formDatum

instance Deform f (Maybe (FileInfo f)) where
  deform = asks formFile

instance Deform f (FileInfo f) where
  deform = deformMaybe' "File upload required" =<< deform

instance Deform f JSON.Value where
  deform = asks (j . formDatum) where
    j FormDatumNone = JSON.Null
    j (FormDatumJSON v) = v
    j (FormDatumBS b) = JSON.String $ TE.decodeUtf8 b
    j FormDatumFlag = JSON.Bool True

-- |'Text' fields are stripped of whitespace, while other string types are not.
instance Deform f T.Text where
  deform = deformParse "" fv where
    fv (FormDatumBS b) = return $ T.strip $ TE.decodeUtf8 b
    fv (FormDatumJSON (JSON.String t)) = return $ T.strip t
    fv (FormDatumJSON (JSON.Number n)) = return $ T.pack $ show n
    fv (FormDatumJSON (JSON.Bool True)) = return "1"
    fv (FormDatumJSON (JSON.Bool False)) = return ""
    fv FormDatumNone = Left "This field is required"
    fv _ = Left "String value required"

instance Deform f BS.ByteString where
  deform = deformParse "" fv where
    fv (FormDatumBS b) = return b
    fv (FormDatumJSON (JSON.String t)) = return $ TE.encodeUtf8 t
    fv (FormDatumJSON (JSON.Number n)) = return $ BSC.pack $ show n
    fv (FormDatumJSON (JSON.Bool True)) = return "1"
    fv (FormDatumJSON (JSON.Bool False)) = return ""
    fv FormDatumNone = Left "This field is required"
    fv _ = Left "String value required"

instance Deform f String where
  deform = deformParse "" fv where
    fv (FormDatumBS b) = return $ BSU.toString b
    fv (FormDatumJSON (JSON.String t)) = return $ T.unpack t
    fv (FormDatumJSON (JSON.Number n)) = return $ show n
    fv (FormDatumJSON (JSON.Bool True)) = return "1"
    fv (FormDatumJSON (JSON.Bool False)) = return ""
    fv FormDatumNone = Left "This field is required"
    fv _ = Left "String value required"

instance Deform f Bool where
  deform = deformParse False fv where
    fv FormDatumNone = return False
    fv (FormDatumBS "true") = return True
    fv (FormDatumBS "false") = return False
    fv (FormDatumBS "on") = return True
    fv (FormDatumBS "off") = return False
    fv (FormDatumBS "1") = return True
    fv (FormDatumBS "0") = return False
    fv (FormDatumBS "") = return False
    fv (FormDatumJSON (JSON.String "true")) = return True
    fv (FormDatumJSON (JSON.String "false")) = return False
    fv (FormDatumJSON (JSON.String "on")) = return True
    fv (FormDatumJSON (JSON.String "off")) = return False
    fv (FormDatumJSON (JSON.String "1")) = return True
    fv (FormDatumJSON (JSON.String "0")) = return False
    fv (FormDatumJSON (JSON.String "")) = return False
    fv (FormDatumJSON (JSON.Number n)) = return $ n /= 0
    fv (FormDatumJSON (JSON.Bool b)) = return b
    fv (FormDatumJSON JSON.Null) = return False
    fv FormDatumFlag = return True
    fv _ = Left "Boolean value required"

instance Deform f Int where
  deform = deformParse 0 fv where
    fv (FormDatumBS b) = maybe (Left "Invalid integer") Right $ do
      (i, r) <- BSC.readInt b
      guard $ BS.null r
      return i
    fv (FormDatumJSON (JSON.String t)) = textInteger t
    fv (FormDatumJSON (JSON.Number n)) = return $ round n
    fv (FormDatumJSON (JSON.Bool True)) = return 1
    fv (FormDatumJSON (JSON.Bool False)) = return 0
    fv FormDatumNone = Left "This field is required"
    fv _ = Left "Integer required"

instance Deform f Int64 where
  deform = deformParse 0 fv where
    fv (FormDatumBS b) = readParser $ BSC.unpack b
    fv (FormDatumJSON (JSON.String t)) = textInteger t
    fv (FormDatumJSON (JSON.Number n)) = return $ round n
    fv (FormDatumJSON (JSON.Bool True)) = return 1
    fv (FormDatumJSON (JSON.Bool False)) = return 0
    fv FormDatumNone = Left "This field is required"
    fv _ = Left "Integer required"

instance Deform f Int32 where
  deform = deformParse 0 fv where
    fv (FormDatumBS b) = readParser $ BSC.unpack b
    fv (FormDatumJSON (JSON.String t)) = textInteger t
    fv (FormDatumJSON (JSON.Number n)) = return $ round n
    fv (FormDatumJSON (JSON.Bool True)) = return 1
    fv (FormDatumJSON (JSON.Bool False)) = return 0
    fv FormDatumNone = Left "This field is required"
    fv _ = Left "Integer required"

instance Deform f Int16 where
  deform = deformParse 0 fv where
    fv (FormDatumBS b) = readParser $ BSC.unpack b
    fv (FormDatumJSON (JSON.String t)) = textInteger t
    fv (FormDatumJSON (JSON.Number n)) = return $ round n
    fv (FormDatumJSON (JSON.Bool True)) = return 1
    fv (FormDatumJSON (JSON.Bool False)) = return 0
    fv FormDatumNone = Left "This field is required"
    fv _ = Left "Integer required"

instance Deform f Date where
  deform = maybe (deformErrorWith (Just (fromGregorian 1900 1 1)) "Invalid date (please use YYYY-MM-DD)") return . pd =<< deform where
    pd t = pf "%Y-%-m-%-d" t <|> pf "%-m/%-d/%y" t
    pf = parseTimeM True defaultTimeLocale

instance Deform f Offset where
  deform = deformParseJSON 0
    $ maybe (Left "Offset required") $ readParser . BSC.unpack

instance Deform f Segment where
  deform = deformParseJSON (Segment Range.Empty)
    $ maybe (Left "Segment required") $ readParser . BSC.unpack

instance Deform f URI where
  deform = maybe (deformErrorWith (Just URI.nullURI) "Invalid URL") return . parseURL =<< deform

readParser :: Read a => String -> Either FormErrorMessage a
readParser = left T.pack . readEither

textInteger :: Integral a => T.Text -> Either FormErrorMessage a
textInteger t = case TR.signed TR.decimal t of
  Left s -> Left (T.pack s)
  Right (i,r)
    | T.null r -> Right i
    | otherwise -> Left ("Trailing \"" <> r `T.snoc` '"')

deformRead :: (Functor m, Monad m) => Read a => a -> DeformT f m a
deformRead def = deformEither def . readParser =<< deform

deformRequired :: (Functor m, Monad m) => T.Text -> DeformT f m T.Text
deformRequired = deformCheck "Required" (not . T.null)
