module Databrary.HTTP.Path.Swagger
  ( swaggerPath
  ) where

import Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Web.Route.Invertible.Internal as R

import Databrary.HTTP.Path

parameter :: T.Text -> T.Text
parameter p = '{' `T.cons` p `T.snoc` '}'

swaggerPath :: PathValues -> [T.Text] -> T.Text
swaggerPath [] [] = T.empty
swaggerPath (R.PlaceholderValueFixed t : l) p = '/' `T.cons` t <> swaggerPath l p
swaggerPath (R.PlaceholderValueParameter _ : l) (a:p) = '/' `T.cons` parameter a <> swaggerPath l p
swaggerPath _ _ = error "swaggerPath parameter mismatch"
