{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Databrary.Model.Enum
  ( DBEnum
  , readDBEnum
  , makeDBEnum
  , parseJSONEnum
  , enumForm
  , pgEnumValues
  ) where

import Control.Arrow (left)
import Control.Monad (liftM2)
import qualified Data.Aeson.Types as JSON
import qualified Data.ByteString.Char8 as BSC
import qualified Data.CaseInsensitive as CI (mk)
import qualified Data.Text as T
import Database.PostgreSQL.Typed.Enum (PGEnum, pgEnumValues, makePGEnum)
import Text.Read (readMaybe)

import qualified Language.Haskell.TH as TH

import Databrary.Service.DB (useTDB)
import Databrary.Model.Kind
import Databrary.HTTP.Form (FormDatum(..))
import Databrary.HTTP.Form.Deform
import Databrary.String

class (PGEnum a, Kinded a) => DBEnum a

readDBEnum :: forall a . DBEnum a => String -> Maybe a
readDBEnum s
  | Just i <- readMaybe s, i >= fe minBound, i <= fe maxBound = Just (toEnum i)
  | [(x, _)] <- filter ((==) s . snd) pgEnumValues = Just x
  | [(x, _)] <- filter ((==) (CI.mk s) . CI.mk . snd) pgEnumValues = Just x
  | otherwise = Nothing
  where
  fe :: a -> Int
  fe = fromEnum

parseJSONEnum :: forall a . DBEnum a => JSON.Value -> JSON.Parser a
parseJSONEnum (JSON.String t) | Just e <- readDBEnum (T.unpack t) = return e
parseJSONEnum (JSON.Number x) = p (round x) where
  p i
    | i < fe minBound || i > fe maxBound = fail $ kindOf (undefined :: a) ++ " out of range"
    | otherwise = return $ toEnum i
  fe :: a -> Int
  fe = fromEnum
parseJSONEnum _ = fail $ "Invalid " ++ kindOf (undefined :: a)

enumForm :: forall a m f . (Functor m, Monad m, DBEnum a) => DeformT f m a
enumForm = deformParse minBound fv where
  fv (FormDatumBS b) = maybe e return $ readDBEnum $ BSC.unpack b
  fv (FormDatumJSON j) = left T.pack $ JSON.parseEither parseJSONEnum j
  fv _ = e
  e = Left $ "Invalid " `T.append` kindOf (undefined :: a)

makeDBEnum :: String -> String -> TH.DecsQ
makeDBEnum name typs = do
  [] <- useTDB
  liftM2 (++)
    (makePGEnum name typs (\s -> typs ++ toCamel s))
    [d| instance Kinded $(return typt) where
          kindOf _ = $(TH.litE $ TH.stringL name)
        instance DBEnum $(return typt)
        instance JSON.ToJSON $(return typt) where
          toJSON = JSON.toJSON . fromEnum
        instance JSON.FromJSON $(return typt) where
          parseJSON = parseJSONEnum
        instance Deform f $(return typt) where
          deform = enumForm
    |]
  where
  typt = TH.ConT (TH.mkName typs)
