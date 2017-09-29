{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Databrary.HTTP.Form
  ( FormKey(..)
  , FormPath
  , formPathText
  , FormData
  , FormDatum(..)
  , Form(..)
  , initForm
  , subForm
  , subForms
  ) where

import qualified Data.Aeson as JSON
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as Map
import Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import Network.Wai.Parse (FileInfo)

import Databrary.Has (Has(..))
import Databrary.HTTP.Form.Data

data FormKey
  = FormField !T.Text
  | FormIndex !Int
  deriving (Eq, Ord)

type FormPath = [FormKey]

formSubPath :: FormKey -> FormPath -> FormPath
formSubPath k p = p ++ [k]

instance Has BS.ByteString FormKey where
  view (FormField t) = TE.encodeUtf8 t
  view (FormIndex i) = BSC.pack $ show i

instance Has T.Text FormKey where
  view (FormField t) = t
  view (FormIndex i) = T.pack $ show i

dotsBS :: [BS.ByteString] -> BS.ByteString
dotsBS = BS.intercalate (BSC.singleton '.')

dotBS :: BS.ByteString -> BS.ByteString -> BS.ByteString
dotBS a b
  | BS.null a = b
  | otherwise = dotsBS [a, b]

formSubBS :: FormKey -> BS.ByteString -> BS.ByteString
formSubBS k b = b `dotBS` view k

formPathText :: FormPath -> T.Text
formPathText = T.intercalate (T.singleton '.') . map view

data FormDatum
  = FormDatumNone
  | FormDatumBS !BS.ByteString
  | FormDatumJSON !JSON.Value
  | FormDatumFlag
  deriving (Eq)

instance Monoid FormDatum where
  mempty = FormDatumNone
  mappend FormDatumNone x = x
  mappend x _ = x

data Form a = Form
  { formData :: !(FormData a)
  , formPath :: FormPath
  , formPathBS :: BS.ByteString
  , formJSON :: Maybe JSON.Value
  , formDatum :: FormDatum
  , formFile :: Maybe (FileInfo a)
  }

-- makeHasRec ''Form ['formData, 'formPath, 'formPathBS, 'formDatum, 'formFile]

initForm :: FormData a -> Form a
initForm d = form where form = Form d [] "" (formDataJSON d) (getFormDatum form) Nothing

formSubJSON :: FormKey -> JSON.Value -> Maybe JSON.Value
formSubJSON k (JSON.Object o) = HM.lookup (view k) o
formSubJSON (FormIndex i) (JSON.Array a) = a V.!? i
formSubJSON _ _ = Nothing

subForm :: FormKey -> Form a -> Form a
subForm key form = form' where
  form' = form
    { formPath = formSubPath key $ formPath form
    , formPathBS = formSubBS key $ formPathBS form
    , formJSON = formSubJSON key =<< formJSON form
    , formDatum = getFormDatum form'
    , formFile = getFormFile form'
    }

jsonSubForms :: Form a -> [(FormKey, Form a)]
jsonSubForms f = maybe [] jfk (formJSON f) where
  jfk (JSON.Array a) = V.toList $ V.imap (sfj . FormIndex) a
  jfk (JSON.Object o) = HM.elems $ HM.mapWithKey (sfj . FormField) o
  jfk _ = []
  sfj k v = (k, (subForm k f){ formJSON = Just v, formDatum = FormDatumJSON v })

subFormsFor :: (FormData a -> Map.Map BS.ByteString b) -> Form a -> [(FormKey, Form a)]
subFormsFor m f =
  map (sf . FormField . TE.decodeUtf8) $ uniq $ map (BSC.takeWhile ('.' /=) . BS.drop l') $ takeWhile (BS.isPrefixOf p') $ Map.keys $ snd $ Map.split p' $ m $ formData f where
  sf k = (k, subForm k f)
  p' = formPathBS f `dotBS` ""
  l' = BS.length p'
  uniq (a:bl@(b:_))
    | a == b = uniq bl
    | otherwise = a : uniq bl
  uniq l = l

subForms :: Form a -> [(FormKey, Form a)]
subForms f = subFormsFor formDataPost f ++ jsonSubForms f ++ subFormsFor formDataQuery f

jsonFormDatum :: Form a -> FormDatum
jsonFormDatum Form{ formJSON = j } = foldMap FormDatumJSON j

queryFormDatum :: Form a -> FormDatum
queryFormDatum Form{ formData = FormData{ formDataQuery = m }, formPathBS = p } =
  foldMap (maybe FormDatumFlag FormDatumBS) $ Map.lookup p m

postFormDatum :: Form a -> FormDatum
postFormDatum Form{ formData = FormData{ formDataPost = m }, formPathBS = p } =
  foldMap FormDatumBS $ Map.lookup p m

getFormDatum :: Form a -> FormDatum
getFormDatum form = postFormDatum form <> jsonFormDatum form <> queryFormDatum form

getFormFile :: Form a -> Maybe (FileInfo a)
getFormFile Form{ formData = FormData{ formDataFiles = f }, formPathBS = p } = Map.lookup p f
