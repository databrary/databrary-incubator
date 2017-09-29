{-# LANGUAGE OverloadedStrings #-}
module Databrary.View.Template
  ( htmlHeader
  , htmlFooter
  , htmlTemplate
  , htmlSocialMedia
  ) where

import Control.Monad (void, when, forM_)
import qualified Data.ByteString.Builder as BSB
import Data.Monoid ((<>))
import qualified Data.Text as T
import Data.Version (showVersion)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as HA
import Network.HTTP.Types (methodGet)
import qualified Network.Wai as Wai

import Paths_databrary (version)
import Databrary.Ops
import Databrary.Has (view)
import Databrary.Model.Identity
import Databrary.Action.Types
import Databrary.Action.Route
import Databrary.Controller.Paths
import Databrary.View.Html

import {-# SOURCE #-} Databrary.Controller.Angular
import {-# SOURCE #-} Databrary.Controller.Root
import {-# SOURCE #-} Databrary.Controller.Login
import {-# SOURCE #-} Databrary.Controller.Party
import {-# SOURCE #-} Databrary.Controller.Web

htmlHeader :: Maybe BSB.Builder -> JSOpt -> H.Html
htmlHeader canon hasjs = do
  forM_ canon $ \c ->
    H.link
      H.! HA.rel "canonical"
      H.! HA.href (builderValue c)
  H.link
    H.! HA.rel "shortcut icon"
    H.! HA.href (builderValue $ actionURL Nothing webFile (Just $ staticPath ["icons", "favicon.png"]) [])
  H.link
    H.! HA.rel "start"
    H.! actionLink viewRoot HTML hasjs
  forM_ ["news", "about", "access", "community"] $ \l -> H.link
    H.! HA.rel l
    H.! HA.href ("//databrary.org/" <> l <> ".html")

htmlSocialMedia :: H.Html
htmlSocialMedia =
  H.p $ do
    let sm n l a =
          H.a H.! HA.href l H.! HA.target "_blank" H.! HA.class_ "img" $
            H.img H.! HA.id n H.! HA.src ("/web/images/social/16px/" <> n <> ".png") H.! HA.alt a
    void "Find us on "
    sm "twitter" "https://twitter.com/databrary" "Twitter"
    void ", "
    sm "facebook" "https://www.facebook.com/databrary" "Facebook"
    void ", "
    sm "linkedin" "https://www.linkedin.com/company/databrary-project" "LinkedIn"
    void ", "
    sm "google-plus" "https://plus.google.com/u/1/111083162045777800330/posts" "Google+"
    void ", and "
    sm "github" "https://github.com/databrary/" "GitHub"
    "."

htmlFooter :: H.Html
htmlFooter = H.footer H.! HA.id "site-footer" H.! HA.class_ "site-footer" $
  H.div H.! HA.class_ "wrap" $
    H.div H.! HA.class_ "row" $ do
      H.ul H.! HA.class_ "site-footer-grants col-desktop-8 col-tablet-5 col-mobile-6" $ do
        H.li $
          H.a H.! HA.href "http://www.nsf.gov/awardsearch/showAward?AWD_ID=1238599&HistoricalAwards=false" $ do
            H.img H.! HA.src "/web/images/grants/nsf.png" H.! HA.class_ "nsf"
            " BCS-1238599"
        H.li $
          H.a H.! HA.href "http://projectreporter.nih.gov/project_info_description.cfm?aid=8531595&icde=15908155&ddparam=&ddvalue=&ddsub=&cr=1&csb=default&cs=ASC" $ do
            H.img H.! HA.src "/web/images/grants/nih.png" H.! HA.class_ "nih"
            " U01-HD-076595"
      H.div H.! HA.class_ "site-footer-social col-desktop-7 col-tablet-4 col-mobile-6" $ do
        htmlSocialMedia
      H.div H.! HA.class_ "site-footer-legal col" $ do
        H.p $ do
          void "Each dataset on Databrary represents an individual work owned by the party who contributed it. Data on Databrary is provided for non-commercial use and is subject to the terms of use outlined in the "
          H.a H.! HA.href "//databrary.org/access/policies/agreement.html" H.! HA.target "_blank" $
            "Databrary Access Agreement"
          void ". ["
          H.string $ showVersion version
          "]"

htmlTemplate :: RequestContext -> Maybe T.Text -> (JSOpt -> H.Html) -> H.Html
htmlTemplate req title body = H.docTypeHtml $ do
  H.head $ do
    htmlHeader canon hasjs
    H.link
      H.! HA.rel "stylesheet"
      H.! actionLink webFile (Just $ StaticPath "all.min.css") ([] :: Query)
    H.title $ do
      mapM_ (\t -> H.toHtml t >> " || ") title
      "Databrary"
  H.body H.! H.customAttribute "vocab" "http://schema.org" $ do
    H.section
      H.! HA.id "toolbar"
      H.! HA.class_ "toolbar"
      $ H.div
        H.! HA.class_ "wrap toolbar-main"
        $ H.div
          H.! HA.class_ "row"
          $ H.nav
            H.! HA.class_ "toolbar-nav no-angular cf"
            $ do
              H.ul
                H.! HA.class_ "inline-block flat cf"
                $ do
                  H.li $ H.a
                    H.! actionLink viewRoot HTML hasjs
                    $ "Databrary"
                  forM_ ["news", "about", "access", "community"] $ \l ->
                    H.li $ H.a H.! HA.href (H.stringValue $ "//databrary.org/" ++ l ++ ".html") $ do
                      H.string l
              H.ul
                H.! HA.class_ "toolbar-user inline-block flat cf"
                $ foldIdentity
                  (H.li $ H.a H.! actionLink viewLogin () hasjs $ "Login")
                  (\_ -> do
                    H.li $ H.a H.! actionLink viewParty (HTML, TargetProfile) hasjs $ "Your Dashboard"
                    H.li $ actionForm postLogout HTML hasjs $
                      H.button
                        H.! HA.class_ "mini"
                        H.! HA.type_ "submit"
                        $ "Logout")
                  $ requestIdentity req
    H.section
      H.! HA.id "main"
      H.! HA.class_ "main"
        $ H.div
          H.! HA.class_ "wrap"
          $ H.div
            H.! HA.class_ "row"
            $ do
              when (hasjs /= JSEnabled) $ forM_ canon $ \c -> H.div $ do
                H.preEscapedString "Our site works best with modern browsers (Firefox, Chrome, Safari &ge;6, IE &ge;10, and others). \
                  \You are viewing the simple version of our site: some functionality may not be available. \
                  \Try switching to the "
                H.a H.! HA.href (builderValue c) $ "modern version"
                " to see if it will work on your browser."
              mapM_ (H.h1 . H.toHtml) title
                H.! HA.class_ "view-title"
              r <- body hasjs
              htmlFooter
              return r
  where
  (hasjs, nojs) = jsURL JSDefault (view req)
  canon = Wai.requestMethod (view req) == methodGet && hasjs == JSDefault ?!> nojs
