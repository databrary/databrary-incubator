{-# LANGUAGE OverloadedStrings #-}
module Databrary.View.Format
  ( htmlFormats
  ) where

import Control.Monad (forM_)
import Data.Function (on)
import Data.List (groupBy, sortBy, intersperse)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as HA

import Databrary.Has (view)
import qualified Databrary.Store.Config as Conf
import Databrary.Model.Format
import Databrary.Service.Messages
import Databrary.Action
import Databrary.View.Html
import Databrary.View.Template

htmlFormats :: RequestContext -> H.Html
htmlFormats req = htmlTemplate req (Just $ msg "help.formats.title") $ \_ ->
  H.article H.! HA.class_ "wrap" $
    H.div H.! HA.class_ "row" $ do
      H.p $ H.preEscapedText $ msg "help.formats.body"
      H.div H.! HA.class_ "col" $
        H.table $
          forM_ (groupBy (onfmt $ ((==) EQ .) . mimeTypeTopCompare) $ sortBy (onfmt compare) allFormats) $ \group ->
            H.tbody $ do
              H.tr H.! HA.class_ "hr hr-big" $
                H.th H.! HA.colspan "2" $ H.text $ msg (Conf.Path ["format", "list", mimeTypeTop (formatMimeType (head group))])
              H.tr H.! HA.class_ "hr" $ do
                H.th H.! HA.class_ "th-right" $ H.text $ msg "format.list.extension"
                H.th $ H.text $ msg "format.list.description"
              forM_ group $ \fmt ->
                H.tr $ do
                  H.td H.! HA.class_ "th-right" $ mapM_ byteStringHtml $ intersperse "," $ formatExtension fmt
                  H.td $ H.text $ formatName fmt
  where
  msg m = getMessage m (view req)
  onfmt = (`on` formatMimeType)
