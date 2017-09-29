{-# LANGUAGE OverloadedStrings #-}
module Databrary.View.Email
  ( emailNewlyAuthorized

  ) where

import Control.Monad (void)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as HA
import Databrary.View.Template

emailTemplate :: H.Html -> H.Html
emailTemplate content = H.docTypeHtml $ do
  H.head $ do
    H.style H.! HA.type_ "text/css" $ do
      H.preEscapedString "p {font-size: 16px; line-height:150%; font-family:Arial; margin: 1.6em 0}\
      \a {color: #CE763B;}\
      \.header,.footer {text-align: center;}\
      \.header {border-bottom: 1px solid #ccc; padding-bottom:16px;}\
      \.footer {border-top: 1px solid #ccc; padding-top:16px;"

  H.body $ do
    H.table $ do
      H.tr $
        H.td H.! HA.class_ "header" $
          H.img H.! HA.src "https://gallery.mailchimp.com/4b4fa9408b40cd79e92f748e5/images/databrary_name.png" H.! HA.style "max-width:600px;" H.! HA.alt "Databrary"
      H.tr $
        H.td $ do
          content
          H.p "Sincerely,"
          H.p $ do
            void "The Databrary Team"
            H.br
            void "Databrary"
            H.br
            void "196 Mercer Street, Suite 807"
            H.br
            void "New York, NY 10012"
            H.br
            void "Office: 212-998-5536"
            H.br
            void "contact@databrary.org"
            H.br
            H.a H.! HA.href "https://www.databrary.org" $ do
              "www.databrary.org"
      H.tr $
        H.td H.! HA.class_ "footer" $ do
          htmlSocialMedia

emailNewlyAuthorized :: H.Html
emailNewlyAuthorized = emailTemplate $ do
  H.p "Dear Nothing,"
  H.p "You have been authorized for Databrary access by <SPONSOR> and can now access all the shared data in the library. You can see how to conduct procedures, find illustrative videos for teaching, increase citations to your work, and repurpose videos to ask new questions outside the scope of the original study. Databrary's unique \"upload-as-you-go\" data management functionality enables easy efficient data management. Moreover, Databrary ensures secure backup and long-term video preservation."
  H.p $ do
    void "We want to let you know about some resources that may help you get started. We've developed a template "
    H.a H.! HA.href "https://databrary.org/access/policies/release-template.html" $ do
      "Databrary Release"
    void " form for obtaining informed consent for sharing from participants. It's completely adaptable and can be added to new or existing IRB protocols."
  H.p "Our data management features allow you to manage, organize, and preserve your videos and associated metadata in a secure web-based library. You can upload data and video files as soon as they are collected so that they are organized and securely backed-up. Data remains private and accessible only to lab members and collaborators until you are ready to share it with the Databrary community. When data are organized and uploaded, sharing is as easy as a click of a button!"
  H.p $ do
    void "We have lots of information and helpful tips in our "
    H.a H.! HA.href "https://databrary.org/access/guide" $ do
      "User Guide"
    void "."
  H.p $ do
    void "Our support team is dedicated to providing assistance to the Databrary community. Please contact us at "
    H.a H.! HA.href "mailto:support@databrary.org" $ do
      "support@databrary.org"
    void " with any questions or for help getting started."
  H.p "We are glad that you are now part of the Databrary community and we're glad to join you in accelerating the pace of discovery."


