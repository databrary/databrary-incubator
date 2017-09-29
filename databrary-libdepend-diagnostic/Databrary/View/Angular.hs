{-# LANGUAGE CPP, OverloadedStrings #-}
module Databrary.View.Angular
  ( htmlAngular
  ) where

import Control.Monad (forM_)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Char8 as BSC
import Data.Default.Class (def)
import Data.Maybe (isJust)
import Data.Monoid ((<>))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as HA

import Databrary.Has (view)
import qualified Databrary.JSON as JSON
import Databrary.Service.Types
import Databrary.Model.Identity
import Databrary.Action.Types
import Databrary.Web (WebFilePath, webFileRelRaw)
import Databrary.Web.Libs (webDeps, cssWebDeps)
import Databrary.Controller.Web
import Databrary.View.Html
import Databrary.View.Template

ngAttribute :: String -> H.AttributeValue -> H.Attribute
ngAttribute = H.customAttribute . H.stringTag . ("ng-" <>)

webURL :: BS.ByteString -> H.AttributeValue
webURL p = actionValue webFile (Just $ StaticPath p) ([] :: Query)

htmlAngular :: Maybe [WebFilePath] -> BSB.Builder -> RequestContext -> H.Html
htmlAngular debug nojs auth = H.docTypeHtml H.! ngAttribute "app" "databraryModule" $ do
  H.head $ do
    htmlHeader Nothing def
    H.noscript $
      H.meta
        H.! HA.httpEquiv "Refresh"
        H.! HA.content (builderValue $ BSB.string8 "0;url=" <> nojs)
    H.meta
      H.! HA.httpEquiv "X-UA-Compatible"
      H.! HA.content "IE=edge"
    H.meta
      H.! HA.name "viewport"
      H.! HA.content "width=device-width, initial-scale=1.0, minimum-scale=1.0"
    H.title
      H.! ngAttribute "bind" (byteStringValue $ "page.display.title + ' || " <> title <> "'")
      $ H.unsafeByteString title
    forM_ [Just "114x114", Just "72x72", Nothing] $ \size ->
      H.link
        H.! HA.rel "apple-touch-icon-precomposed"
        H.! HA.href (webURL $ "icons/apple-touch-icon" <> maybe "" (BSC.cons '-') size <> ".png")
        !? (HA.sizes . byteStringValue <$> size)
    forM_ (if isJust debug then cssWebDeps True else ["all.min.css"]) $ \css ->
      H.link
        H.! HA.rel "stylesheet"
        H.! HA.href (webURL $ webFileRelRaw css)
    H.script $ do
      H.preEscapedString "(function(w,d,s,l,i){w[l]=w[l]||[];w[l].push({'gtm.start': new Date().getTime(),event:'gtm.js'});var f=d.getElementsByTagName(s)[0], j=d.createElement(s),dl=l!='dataLayer'?'&l='+l:'';j.async=true;j.src= 'https://www.googletagmanager.com/gtm.js?id='+i+dl;f.parentNode.insertBefore(j,f); })(window,document,'script','dataLayer','GTM-NW6PSFL');"
    H.script $ do
      H.preEscapedString "(function(h,o,t,j,a,r){ h.hj=h.hj||function(){(h.hj.q=h.hj.q||[]).push(arguments)}; h._hjSettings={hjid:547484,hjsv:5}; a=o.getElementsByTagName('head')[0];r=o.createElement('script');r.async=1; r.src=t+h._hjSettings.hjid+j+h._hjSettings.hjsv; a.appendChild(r);    })(window,document,'//static.hotjar.com/c/hotjar-','.js?sv=');"
    H.script $ do
      H.preEscapedString "window.$play={user:"
      unsafeBuilder $ JSON.fromEncoding $ JSON.recordEncoding $ identityJSON (view auth)
      forM_ (serviceDown (view auth)) $ \msg -> do
        H.preEscapedString ",down:"
        H.unsafeLazyByteString $ JSON.encode msg
      H.preEscapedString "};"
    forM_ (maybe ["all.min.js"] ((webDeps True ++) . ("debug.js" :)) debug) $ \js ->
      H.script
        H.! HA.src (webURL $ webFileRelRaw js)
        $ return ()
  H.body
    H.! H.customAttribute "flow-prevent-drop" mempty
    $ do
    H.noscript $ do
      H.preEscapedString "Our site works best with modern browsers (Firefox, Chrome, Safari &ge;6, IE &ge;10, and others) with Javascript enabled.  You can also switch to the "
      H.a
        H.! HA.href (builderValue nojs)
        $ "simple version"
      H.preEscapedString " of this page."
    H.preEscapedString "<toolbar></toolbar>"
    H.preEscapedString $ "<main ng-view id=\"main\" class=\"main"
#ifdef SANDBOX
      <> " sandbox"
#endif
      <> "\" autoscroll ng-if=\"!page.display.error\"></main>"
    H.preEscapedString "<errors></errors>"
    htmlFooter
    H.preEscapedString "<messages></messages>"
    H.preEscapedString "<tooltip ng-repeat=\"tooltip in page.tooltips.list\"></tooltip>"
    H.div
      H.! HA.id "loading"
      H.! HA.class_ "loading"
      H.! HA.style "display:none"
      H.! ngAttribute "show" "page.display.loading" $
      H.div H.! HA.class_ "loading-animation" $ do
        H.div H.! HA.class_ "loading-spinner" $
          H.div H.! HA.class_ "loading-mask" $
            H.div H.! HA.class_ "loading-circle" $
              return ()
        H.div H.! HA.class_ "loading-text" $
          "[" >> H.span "loading" >> "]"
    H.script
      $ H.preEscapedString "document.getElementById('loading').style.display='block';"
  where
  title =
#ifdef SANDBOX
    "Databrary Demo"
#else
    "Databrary"
#endif
