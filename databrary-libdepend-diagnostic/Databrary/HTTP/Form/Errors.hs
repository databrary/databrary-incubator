{-# LANGUAGE OverloadedStrings #-}
module Databrary.HTTP.Form.Errors
  ( FormErrorMessage
  , FormErrors
  , formErrors
  , allFormErrors
  , nullFormErrors
  , unsubFormErrors
  , unsubFormsErrors
  , singletonFormError
  , subFormErrors
  , subFormsErrors
  , setSubFormErrors
  ) where

import Control.Arrow (first)
import qualified Data.Aeson as JSON
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as Map
import Data.Monoid ((<>))
import qualified Data.Text as T

import Databrary.Has (view)
import Databrary.HTTP.Form

type FormErrorMessage = T.Text
data FormErrors = FormErrors
  { formErrors :: [FormErrorMessage]
  , _subFormErrors :: Map.Map FormKey FormErrors
  }

instance Monoid FormErrors where
  mempty = FormErrors [] Map.empty
  mappend (FormErrors t1 s1) (FormErrors t2 s2) =
    FormErrors (t1 ++ t2) (Map.unionWith mappend s1 s2)

nullFormErrors :: FormErrors -> Bool
nullFormErrors (FormErrors [] s) = Map.null s
nullFormErrors _ = False

unsubFormErrors :: FormKey -> FormErrors -> FormErrors
unsubFormErrors k e
  | nullFormErrors e = mempty
  | otherwise = FormErrors [] $ Map.singleton k e

unsubFormsErrors :: [FormErrors] -> FormErrors
unsubFormsErrors = FormErrors [] . sf 0 where
  sf _ [] = Map.empty
  sf i (e:l)
    | nullFormErrors e = m
    | otherwise = Map.insert (FormIndex i) e m
    where m = sf (succ i) l

subFormErrors :: FormKey -> FormErrors -> FormErrors
subFormErrors k (FormErrors _ s) = Map.findWithDefault mempty k s

subFormsErrors :: FormErrors -> [FormErrors]
subFormsErrors (FormErrors _ s) = zs 0 (maybe id ((:) . (,) (FormIndex 0)) i0 $ Map.toAscList is) where
  zs _ [] = []
  zs i l@((FormIndex j,e):r)
    | i == j = e : zs (succ i) r
    | i < j = mempty : zs (succ i) l
  zs _ _ = error "subFormsErrors"
  (_, i0, is) = Map.splitLookup (FormIndex 0) s

setSubFormErrors :: FormErrors -> FormKey -> FormErrors -> FormErrors
setSubFormErrors (FormErrors e m) k s = FormErrors e $
  (if nullFormErrors s then Map.delete k else Map.insert k s) m

singletonFormError :: FormErrorMessage -> FormErrors
singletonFormError e = FormErrors [e] Map.empty

allFormErrors :: FormErrors -> [(FormPath, FormErrorMessage)]
allFormErrors (FormErrors l m) =
  map ((,) []) l ++ Map.foldMapWithKey (\k -> map (first (k:)) . allFormErrors) m

subToJSON :: JSON.Object -> Map.Map FormKey FormErrors -> JSON.Value
subToJSON z = JSON.Object . Map.foldrWithKey (\k -> HM.insert (view k) . JSON.toJSON) z

subToEncoding :: Map.Map FormKey FormErrors -> JSON.Series
subToEncoding = Map.foldMapWithKey ((JSON..=) . view)

topToJSON :: [FormErrorMessage] -> JSON.Value
topToJSON [] = JSON.Null
topToJSON [e] = JSON.toJSON e
topToJSON l = JSON.toJSON l

instance JSON.ToJSON FormErrors where
  toJSON (FormErrors [] m) =
    subToJSON HM.empty m
  toJSON (FormErrors l m)
    | Map.null m = top
    | otherwise = subToJSON (HM.singleton "" top) m
    where top = topToJSON l
  toEncoding (FormErrors [] m) =
    JSON.pairs $ subToEncoding m
  toEncoding (FormErrors l m)
    | Map.null m = JSON.toEncoding top
    | otherwise = JSON.pairs $ "" JSON..= top <> subToEncoding m
    where top = topToJSON l
