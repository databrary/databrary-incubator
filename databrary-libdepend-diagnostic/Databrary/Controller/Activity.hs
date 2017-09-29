{-# LANGUAGE OverloadedStrings #-}
module Databrary.Controller.Activity
  ( viewSiteActivity
  , viewPartyActivity
  , viewVolumeActivity
  , viewContainerActivity
  ) where

import Control.Arrow (second)
import Control.Monad (when)
import Data.Function (on)
import Data.IORef (readIORef)
import Data.List (nubBy)
import Data.Maybe (isJust, mapMaybe)
import Data.Monoid ((<>))
import Data.Ord (comparing)

import Databrary.Ops
import Databrary.Has
import qualified Databrary.JSON as JSON
import Databrary.Service.Types
import Databrary.Model.Id
import Databrary.Model.Permission
import Databrary.Model.Party
import Databrary.Model.Authorize
import Databrary.Model.Volume
import Databrary.Model.VolumeAccess
import Databrary.Model.Slot
import Databrary.Model.Activity
import Databrary.HTTP.Path.Parser
import Databrary.Action
import Databrary.Controller.Paths
import Databrary.Controller.Angular
import Databrary.Controller.Party
import Databrary.Controller.Volume
import Databrary.Controller.Container
import Databrary.View.Activity

viewSiteActivity :: ActionRoute API
viewSiteActivity = action GET (pathAPI </< "activity") $ \api -> withAuth $ do
  ss <- focusIO $ readIORef . serviceStats
  vl <- map (second $ ("volume" JSON..=:) . volumeJSON) . nubBy ((==) `on` volumeId . volumeRow . snd) <$> lookupVolumeShareActivity 8
  al <- map (second $ ("party"  JSON..=:) . partyJSON)  . nubBy ((==) `on` partyId  . partyRow  . snd) <$> lookupAuthorizeActivity 8
  case api of
    JSON -> return $ okResponse [] $ JSON.objectEncoding $
         "stats" JSON..= ss
      <> JSON.nestObject "activity" (\u -> map (u . ent) (take 12 $ mergeBy ((fo .) . comparing fst) vl al))
    HTML -> peeks $ okResponse [] . htmlSiteActivity ss
  where
  ent (t, j) = j <> "time" JSON..= t
  fo GT = LT
  fo _ = GT

viewPartyActivity :: ActionRoute (API, PartyTarget)
viewPartyActivity = action GET (pathAPI </> pathPartyTarget </< "activity") $ \(api, p) -> withAuth $ do
  when (api == HTML) angular
  v <- getParty (Just PermissionADMIN) p
  a <- lookupPartyActivity v
  return $ case api of
    ~JSON -> okResponse [] $ JSON.toEncoding $ mapMaybe activityJSON a
    -- TODO: HTML

viewVolumeActivity :: ActionRoute (API, Id Volume)
viewVolumeActivity = action GET (pathAPI </> pathId </< "activity") $ \(api, vi) -> withAuth $ do
  when (api == HTML) angular
  v <- getVolume PermissionEDIT vi
  a <- lookupVolumeActivity v
  return $ case api of
    ~JSON -> okResponse [] $ JSON.toEncoding $ mapMaybe activityJSON a
    -- TODO: HTML

viewContainerActivity :: ActionRoute (API, (Maybe (Id Volume), Id Slot))
viewContainerActivity = action GET (pathAPI </> pathMaybe pathId </> pathSlotId </< "activity") $ \(api, (vi, ci)) -> withAuth $ do
  when (api == HTML && isJust vi) angular
  v <- getContainer PermissionEDIT vi ci True
  a <- lookupContainerActivity v
  return $ case api of
    ~JSON -> okResponse [] $ JSON.toEncoding $ mapMaybe activityJSON a
    -- TODO: HTML
