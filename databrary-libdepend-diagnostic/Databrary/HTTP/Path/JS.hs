module Databrary.HTTP.Path.JS
  ( jsPath
  ) where

import Control.Arrow ((***), second)
import Control.Monad (liftM2)
import qualified Data.ByteString.Builder as B
import Data.Char (isAscii, isAlphaNum, toLower)
import Data.List (intersperse)
import Data.Monoid ((<>))
import qualified Data.Text.Encoding as TE
import Data.Typeable (typeOf)
import qualified Web.Route.Invertible.Internal as R

import Databrary.JSON (escapeByteString)
import Databrary.HTTP.Path

elementArgs :: PathValues -> ([B.Builder], B.Builder)
elementArgs [] = ([], B.string8 "\"/\"")
elementArgs el = second (\r -> bq <> r <> bq) $ ea 0 el where
  ea i (R.PlaceholderValueFixed t:l) = second ((bs <> escapeByteString jq (TE.encodeUtf8 t)) <>) $ ea i l
  ea i (e:l) = (a :) *** (mconcat [bs, bq, bp, a, bp, bq] <>) $ ea (succ i) l
    where a = B.string8 (av e) <> B.intDec i
  ea _ [] = ([], mempty)
  av (R.PlaceholderValueParameter a) = tl $ filter (liftM2 (&&) isAscii isAlphaNum) $ show $ typeOf a
  av _ = "path"
  tl [] = "a"
  tl (c:l) = toLower c : l
  bq = B.char8 jq
  bp = B.char8 '+'
  bs = B.char8 '/'
  jq = '"'

jsPath :: PathValues -> B.Builder
jsPath el = B.string8 "function(" <> mconcat (intersperse (B.char8 ',') args) <>
  B.string8 "){return " <> expr <> B.string8 ";}"
  where
  (args, expr) = elementArgs el
