module Databrary.String
  ( fromCamel
  , toCamel
  ) where

import Data.Char (isUpper, toLower, toUpper)

fromCamel :: String -> String
fromCamel "" = ""
fromCamel (c:s) = toLower c:fromCamel' s

fromCamel' :: String -> String
fromCamel' "" = ""
fromCamel' cs@(c:s)
  | isUpper c = '_':fromCamel cs
  | otherwise = c:fromCamel' s

toCamel :: String -> String
toCamel "" = ""
toCamel (c:s) = toUpper c:toCamel' s

toCamel' :: String -> String
toCamel' "" = ""
toCamel' ('_':s) = toCamel s
toCamel' (c:s) = c:toCamel' s
