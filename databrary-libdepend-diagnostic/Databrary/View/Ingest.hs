{-# LANGUAGE OverloadedStrings #-}
module Databrary.View.Ingest
  ( htmlIngestForm
  ) where

import Control.Monad (void, forM_)
import qualified Data.Aeson as JSON
import Data.Monoid ((<>))
import qualified Text.Blaze.Html5 as H

import Databrary.Action.Types
import Databrary.Action.Route
import Databrary.Model.Id.Types
import Databrary.Model.Volume
import Databrary.Ingest.Service
import Databrary.View.Html
import Databrary.View.Form

import Databrary.Controller.Angular
import Databrary.Controller.Container
import {-# SOURCE #-} Databrary.Controller.Ingest

htmlIngestStatus :: IngestStatus -> JSOpt -> H.Html
htmlIngestStatus IngestInactive _ = mempty
htmlIngestStatus (IngestActive _) _ = void "An ingest is currently running..."
htmlIngestStatus (IngestFailed el) _ = do
  void "Previous ingest failed:"
  H.ul $ mapM_ (H.li . H.text) el
htmlIngestStatus (IngestCompleted cl) js = do
  void "Previous ingest completed:"
  H.ul $ forM_ cl $ \c ->
    H.li $ H.a H.! actionLink viewContainer (HTML, (Nothing, Id c)) js $ H.toMarkup c

htmlIngestForm :: Volume -> IngestStatus -> RequestContext -> FormHtml JSON.Value
htmlIngestForm v s = htmlForm
  ("Ingest " <> volumeName (volumeRow v))
  postIngest (volumeId $ volumeRow v)
  (case s of
    IngestActive _ ->
      field "abort" $ inputCheckbox False
    _ -> do
      field "run" $ inputCheckbox False
      field "overwrite" $ inputCheckbox False
      field "json" $ inputFile)
  (htmlIngestStatus s)
