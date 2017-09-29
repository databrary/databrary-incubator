{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Databrary.View.Transcode
  ( htmlTranscodes
  ) where

import Control.Monad (when, forM_)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as HA

import Databrary.Has (view)
import Databrary.Model.Transcode
import Databrary.Model.Asset
import Databrary.Model.Party
import Databrary.Action
import Databrary.Controller.Paths
import Databrary.View.Html
import Databrary.View.Template

import Databrary.Controller.Asset
import Databrary.Controller.Party
import {-# SOURCE #-} Databrary.Controller.Transcode

htmlTranscodes :: [Transcode] -> RequestContext -> H.Html
htmlTranscodes tl req = htmlTemplate req (Just "transcodes") $ \js -> do
  H.table $ do
    H.thead $ H.tr $
      mapM_ H.th
        [ "action"
        , "id"
        , "time"
        , "owner"
        , "source"
        , "segment"
        , "options"
        , "pid"
        , "log"
        ]
    H.tbody $
      forM_ tl $ \t@Transcode{..} -> H.tr $ do
        H.td $ actionForm postTranscode (transcodeId t) js $ do
          let act a = H.input H.! HA.type_ "submit" H.! HA.name "action" H.! HA.value (H.stringValue $ show a)
          maybe (do
            act TranscodeStart
            act TranscodeFail)
            (\p -> when (p >= 0) $ act TranscodeStop)
            transcodeProcess
        H.td $ H.a H.! actionLink viewAsset (HTML, assetId $ assetRow $ transcodeAsset t) js $
          H.string $ show $ assetId $ assetRow $ transcodeAsset t
        H.td $ foldMap (H.string . show) transcodeStart
        H.td $ do
          let p = view transcodeOwner
          H.a H.! actionLink viewParty (HTML, TargetParty (partyId p)) js $
            H.text $ partyName p
        H.td $ H.a H.! actionLink viewAsset (HTML, assetId $ assetRow $ transcodeOrig t) js $
          maybe (H.string $ show $ assetId $ assetRow $ transcodeOrig t) H.text (assetName $ assetRow $ transcodeOrig t)
        H.td $ H.string $ show transcodeSegment
        H.td $ mapM_ ((>>) " " . H.string) transcodeOptions
        H.td $ foldMap (H.string . show) transcodeProcess
        H.td $ foldMap (H.pre . byteStringHtml) transcodeLog
