{-# LANGUAGE OverloadedStrings #-}
module Databrary.Controller.Ingest
  ( viewIngest
  , postIngest
  ) where

import Control.Arrow (right)
import Control.Monad (unless)
import Network.HTTP.Types (badRequest400)
import Network.Wai.Parse (FileInfo(..))
import System.Posix.FilePath (takeExtension)

import Databrary.Ops
import Databrary.Has
import Databrary.Model.Id
import Databrary.Model.Permission
import Databrary.Model.Volume
import Databrary.Model.Container
import Databrary.Ingest.Action
import Databrary.Ingest.JSON
import Databrary.HTTP.Path.Parser
import Databrary.HTTP.Form.Deform
import Databrary.Action.Route
import Databrary.Action
import Databrary.Controller.Paths
import Databrary.Controller.Permission
import Databrary.Controller.Form
import Databrary.Controller.Volume
import Databrary.View.Ingest

viewIngest :: ActionRoute (Id Volume)
viewIngest = action GET (pathId </< "ingest") $ \vi -> withAuth $ do
  checkMemberADMIN
  s <- focusIO getIngestStatus
  v <- getVolume PermissionEDIT vi
  peeks $ blankForm . htmlIngestForm v s

postIngest :: ActionRoute (Id Volume)
postIngest = multipartAction $ action POST (pathId </< "ingest") $ \vi -> withAuth $ do
  checkMemberADMIN
  s <- focusIO getIngestStatus
  v <- getVolume PermissionEDIT vi
  a <- runFormFiles [("json", 16*1024*1024)] (Just $ htmlIngestForm v s) $ do
    csrfForm
    abort <- "abort" .:> deform
    abort ?!$> (,,)
      <$> ("run" .:> deform)
      <*> ("overwrite" .:> deform)
      <*> ("json" .:>
        (deformCheck "Must be JSON." (\f ->
          fileContentType f `elem` ["text/json", "application/json"] || takeExtension (fileName f) == ".json")
        =<< deform))
  r <- maybe
    (True <$ focusIO abortIngest)
    (\(r,o,j) -> runIngest $ right (map (unId . containerId . containerRow)) <$> ingestJSON v (fileContent j) r o)
    a
  unless r $ result $ response badRequest400 [] ("failed" :: String)
  peeks $ otherRouteResponse [] viewIngest (volumeId $ volumeRow v)
