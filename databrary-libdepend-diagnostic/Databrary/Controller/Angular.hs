{-# LANGUAGE CPP, OverloadedStrings #-}
module Databrary.Controller.Angular
  ( JSOpt(..)
  , jsURL
  , angular
  ) where

import Control.Arrow (second)
#ifdef DEVEL
import Control.Monad.IO.Class (liftIO)
#endif
import qualified Data.ByteString.Builder as BSB
import Data.Default.Class (Default(..))
import Network.HTTP.Types (hUserAgent, QueryLike(..))
import qualified Network.Wai as Wai
import qualified Text.Regex.Posix as Regex

import Databrary.Ops
import Databrary.Has
#ifdef DEVEL
import Databrary.Web.Uglify
#endif
import Databrary.Action
import Databrary.HTTP (encodePath')
import Databrary.HTTP.Request
import Databrary.View.Angular

data JSOpt
  = JSDisabled
  | JSDefault
  | JSEnabled
  deriving (Eq, Ord)

instance Default JSOpt where
  def = JSDefault

instance Monoid JSOpt where
  mempty = JSDefault
  mappend JSDefault j = j
  mappend j _ = j

instance QueryLike JSOpt where
  toQuery JSDisabled = [("js", Just "0")]
  toQuery JSDefault = []
  toQuery JSEnabled = [("js", Just "1")]

jsEnable :: Bool -> JSOpt
jsEnable False = JSDisabled
jsEnable True = JSEnabled

jsURL :: JSOpt -> Wai.Request -> (JSOpt, BSB.Builder)
jsURL js req =
  second (encodePath' (Wai.pathInfo req) . (toQuery js ++))
  $ unjs $ Wai.queryString req where
  unjs [] = (JSDefault, [])
  unjs (("js",v):q) = (jsEnable (boolParameterValue v), snd $ unjs q)
  unjs (x:q) = second (x:) $ unjs q

browserBlacklist :: Regex.Regex
browserBlacklist = Regex.makeRegex
  ("^Mozilla/.* \\(.*\\<(MSIE [0-9]\\.[0-9]|AppleWebKit/.* Version/[0-5]\\..* Safari/)" :: String)

angularEnable :: JSOpt -> Wai.Request -> Bool
angularEnable JSDisabled = const False
angularEnable JSDefault = not . any (Regex.matchTest browserBlacklist) . lookupRequestHeader hUserAgent
angularEnable JSEnabled = const True

angularRequest :: Wai.Request -> Maybe BSB.Builder
angularRequest req = angularEnable js req ?> nojs
  where (js, nojs) = jsURL JSDisabled req

angularResult :: BSB.Builder -> RequestContext -> IO ()
angularResult nojs auth = do
  debug <-
#ifdef DEVEL
    boolQueryParameter "debug" (view auth) ?$> liftIO appWebJS
#else
    return Nothing
#endif
  result $ okResponse [] (htmlAngular debug nojs auth)

angular :: ActionM ()
angular = mapM_ (focusIO . angularResult) =<< peeks angularRequest
