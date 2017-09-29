{-# LANGUAGE OverloadedStrings #-}
module Databrary.View.Container
  ( releaseTitle
  , htmlContainerEdit
  ) where

import qualified Data.ByteString.Char8 as BSC
import Data.Foldable (fold)
import Data.Monoid ((<>))
import qualified Data.Text as T

import qualified Databrary.Store.Config as C
import Databrary.Model.Volume.Types
import Databrary.Model.Container.Types
import Databrary.Model.Slot.Types
import Databrary.Model.Release.Types
import Databrary.Service.Messages
import Databrary.Action.Types
import Databrary.Action
import Databrary.View.Form

import {-# SOURCE #-} Databrary.Controller.Container

releaseTitle :: Maybe Release -> Messages -> T.Text
releaseTitle rel = getMessage $ C.Path ["release", maybe "UNRELEASED" (BSC.pack . show) rel, "title"]

htmlContainerForm :: Maybe Container -> FormHtml f
htmlContainerForm cont = do
  field "name" $ inputText (containerName . containerRow =<< cont)
  field "date" $ inputDate (containerDate . containerRow =<< cont)
  field "release" $ inputEnum False (containerRelease =<< cont)

htmlContainerEdit :: Either Volume Container -> RequestContext -> FormHtml f
htmlContainerEdit (Left v)  = htmlForm "Create container" createContainer (HTML, volumeId $ volumeRow v) (htmlContainerForm Nothing) (const mempty)
htmlContainerEdit (Right c) = htmlForm ("Edit container " <> fold (containerName $ containerRow c)) postContainer (HTML, containerSlotId $ containerId $ containerRow c) (htmlContainerForm $ Just c) (const mempty)
