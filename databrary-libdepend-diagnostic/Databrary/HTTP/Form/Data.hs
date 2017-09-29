module Databrary.HTTP.Form.Data
  ( FormData(..)
  ) where

import Control.Applicative ((<|>))
import qualified Data.Aeson as JSON
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import Network.Wai.Parse (FileInfo)

data FormData a = FormData
  { formDataQuery :: Map.Map BS.ByteString (Maybe BS.ByteString)
  , formDataPost :: Map.Map BS.ByteString BS.ByteString
  , formDataJSON :: Maybe JSON.Value
  , formDataFiles :: Map.Map BS.ByteString (FileInfo a)
  }

instance Monoid (FormData a) where
  mempty = FormData mempty mempty Nothing mempty
  mappend (FormData q1 p1 j1 f1) (FormData q2 p2 j2 f2) =
    FormData (mappend q1 q2) (mappend p1 p2) (j1 <|> j2) (mappend f1 f2)

