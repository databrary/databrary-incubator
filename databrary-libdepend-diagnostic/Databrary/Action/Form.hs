module Databrary.Action.Form
  ( getFormData
  ) where

import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Word (Word64)
import qualified Network.Wai as Wai

import Databrary.Has (peeks)
import Databrary.HTTP.Form.Data
import Databrary.HTTP.Parse
import Databrary.Action.Types

getFormData :: FileContent a => [(BS.ByteString, Word64)] -> ActionM (FormData a)
getFormData fields = do
  f <- peeks $ FormData . Map.fromList . Wai.queryString
  c <- parseRequestContent (fromMaybe 0 . (`lookup` fields))
  return $ case c of
    ContentForm p u -> f (Map.fromList p) Nothing (Map.fromList u)
    ContentJSON j -> f Map.empty (Just j) Map.empty
    _ -> f Map.empty Nothing Map.empty

