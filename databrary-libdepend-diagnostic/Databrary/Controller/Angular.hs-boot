module Databrary.Controller.Angular where

import qualified Data.ByteString.Builder as BSB
import Data.Default.Class (Default)
import Network.HTTP.Types.QueryLike (QueryLike)
import qualified Network.Wai as Wai

data JSOpt
  = JSDisabled
  | JSDefault
  | JSEnabled
instance Eq JSOpt
instance Ord JSOpt
instance Default JSOpt
instance QueryLike JSOpt

jsURL :: JSOpt -> Wai.Request -> (JSOpt, BSB.Builder)
