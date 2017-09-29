{-# LANGUAGE OverloadedStrings #-}
module Databrary.View.Html
  ( unsafeBuilder
  , lazyByteStringHtml
  , byteStringHtml
  , builderHtml
  , unsafeBuilderValue
  , lazyByteStringValue
  , byteStringValue
  , builderValue
  , actionValue
  , actionLink
  , Query
  , actionForm
  , (!?)
  ) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Lazy as BSL
import Network.HTTP.Types (Query, QueryLike(..))
import qualified Text.Blaze.Internal as Markup
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as HA
import qualified Web.Route.Invertible as R

import Blaze.ByteString.Builder.Html.Word (fromHtmlEscapedByteString, fromHtmlEscapedLazyByteString)
import Databrary.Action.Route
import Databrary.HTTP.Route

import {-# SOURCE #-} Databrary.Controller.Angular

unsafeBuilder :: BSB.Builder -> H.Markup
unsafeBuilder = H.unsafeLazyByteString . BSB.toLazyByteString

lazyByteStringHtml :: BSL.ByteString -> H.Markup
lazyByteStringHtml = unsafeBuilder . fromHtmlEscapedLazyByteString

byteStringHtml :: BS.ByteString -> H.Markup
byteStringHtml = unsafeBuilder . fromHtmlEscapedByteString

builderHtml :: BSB.Builder -> H.Markup
builderHtml = lazyByteStringHtml . BSB.toLazyByteString

unsafeBuilderValue :: BSB.Builder -> H.AttributeValue
unsafeBuilderValue = H.unsafeLazyByteStringValue . BSB.toLazyByteString

lazyByteStringValue :: BSL.ByteString -> H.AttributeValue
lazyByteStringValue = unsafeBuilderValue . fromHtmlEscapedLazyByteString

byteStringValue :: BS.ByteString -> H.AttributeValue
byteStringValue = unsafeBuilderValue . fromHtmlEscapedByteString

builderValue :: BSB.Builder -> H.AttributeValue
builderValue = lazyByteStringValue . BSB.toLazyByteString

actionValue :: QueryLike q => R.RouteAction r a -> r -> q -> H.AttributeValue
actionValue r a q = builderValue $ actionURL Nothing r a $ toQuery q

actionLink :: QueryLike q => R.RouteAction r a -> r -> q -> H.Attribute
actionLink r a = HA.href . actionValue r a

actionForm :: Route r a -> a -> JSOpt -> H.Html -> H.Html
actionForm r a j = H.form
  H.! HA.method (H.unsafeByteStringValue $ R.renderParameter $ R.requestMethod rr)
  H.!? (not $ BS.null $ R.requestContentType rr, HA.enctype $ byteStringValue $ R.requestContentType rr)
  H.! HA.action (builderValue $ routeURL Nothing rr (toQuery j))
  where rr = R.requestActionRoute r a

(!?) :: Markup.Attributable h => h -> Maybe H.Attribute -> h
h !? Nothing = h
h !? (Just a) = h H.! a
