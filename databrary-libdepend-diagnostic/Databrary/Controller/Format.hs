{-# LANGUAGE OverloadedStrings #-}
module Databrary.Controller.Format
  ( viewFormats
  , formatIcon
  ) where

import Control.Monad.Reader (asks)
import qualified Data.Invertible as I
import Data.Monoid ((<>))
import System.Posix.FilePath (splitFileName, splitExtension)
import qualified Web.Route.Invertible as R

import Databrary.Model.Format
import Databrary.HTTP.Path.Parser
import Databrary.Action.Run
import Databrary.Action
import Databrary.Controller.Web
import Databrary.Controller.Angular
import Databrary.View.Format

formatIcon :: ActionRoute Format
formatIcon = (pf I.:<->: fp) `R.mapActionRoute` webFile where
  fp f = Just $ staticPath
    [ "images", "filetype", "16px"
    , case formatExtension f of { e:_ -> e ; _ -> "_blank" } <> ".svg"
    ]
  pf (Just (StaticPath p))
    | ("images/filetype/16px/", i) <- splitFileName p
    , (e, ".svg") <- splitExtension i
    , Just f <- getFormatByExtension e = f
  pf _ = unknownFormat

viewFormats :: ActionRoute ()
viewFormats = action GET ("asset" >/> "formats") $ \() -> withoutAuth $ do
  angular
  okResponse [] <$> asks htmlFormats
