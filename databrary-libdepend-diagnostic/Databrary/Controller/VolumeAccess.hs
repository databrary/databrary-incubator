{-# LANGUAGE OverloadedStrings #-}
module Databrary.Controller.VolumeAccess
  ( viewVolumeAccess
  , postVolumeAccess
  ) where

import Control.Monad (when, forM_)
import Data.Function (on)

import Databrary.Ops
import Databrary.Has
import qualified Databrary.JSON as JSON
import Databrary.Model.Id
import Databrary.Model.Permission
import Databrary.Model.Identity
import Databrary.Model.Volume
import Databrary.Model.VolumeAccess
import Databrary.Model.Party
import Databrary.Model.Notification.Types
import Databrary.HTTP.Form.Deform
import Databrary.HTTP.Path.Parser
import Databrary.Action
import Databrary.Controller.Paths
import Databrary.Controller.Form
import Databrary.Controller.Volume
import Databrary.Controller.Notification
import Databrary.View.VolumeAccess

viewVolumeAccess :: ActionRoute (Id Volume, VolumeAccessTarget)
viewVolumeAccess = action GET (pathHTML >/> pathId </> pathVolumeAccessTarget) $ \(vi, VolumeAccessTarget ap) -> withAuth $ do
  v <- getVolume PermissionADMIN vi
  a <- maybeAction =<< lookupVolumeAccessParty v ap
  peeks $ blankForm . htmlVolumeAccessForm a

postVolumeAccess :: ActionRoute (API, (Id Volume, VolumeAccessTarget))
postVolumeAccess = action POST (pathAPI </> pathId </> pathVolumeAccessTarget) $ \(api, arg@(vi, VolumeAccessTarget ap)) -> withAuth $ do
  v <- getVolume (if ap == partyId (partyRow staffParty) then PermissionEDIT else PermissionADMIN) vi
  a <- maybeAction =<< lookupVolumeAccessParty v ap
  u <- peek
  let su = identityAdmin u
      ru = unId ap > 0
  a' <- runForm (api == HTML ?> htmlVolumeAccessForm a) $ do
    csrfForm
    delete <- "delete" .:> deform
    let del
          | delete = return PermissionNONE
          | otherwise = deform
    individual <- "individual" .:> (del
      >>= deformCheck "Cannot share full access." ((||) ru . (PermissionSHARED >=))
      >>= deformCheck "Cannot remove your ownership." ((||) (su || not (volumeAccessProvidesADMIN a)) . (PermissionADMIN <=)))
    children <- "children" .:> (del
      >>= deformCheck "Inherited access must not exceed individual." (individual >=)
      >>= deformCheck "You are not authorized to share data." ((||) (ru || accessSite u >= PermissionEDIT) . (PermissionNONE ==)))
    sort <- "sort" .:> deformNonEmpty deform
    return a
      { volumeAccessIndividual = individual
      , volumeAccessChildren = children
      , volumeAccessSort = sort
      }
  r <- changeVolumeAccess a'
  if ap == partyId (partyRow rootParty) && on (/=) volumeAccessChildren a' a
    then do
      createVolumeNotification v $ \n -> (n NoticeVolumeSharing)
        { notificationPermission = Just $ volumeAccessChildren a'
        }
      broadcastNotification (volumeAccessChildren a' >= PermissionSHARED) $ \n -> (n NoticeSharedVolume)
        { notificationVolume = Just $ volumeRow v
        }
    else when (ru && on (/=) volumeAccessIndividual a' a) $ do
      createVolumeNotification v $ \n -> (n NoticeVolumeAccessOther)
        { notificationParty = Just $ partyRow $ volumeAccessParty a'
        , notificationPermission = Just $ volumeAccessIndividual a'
        }
      when (ap /= view u) $ forM_ (partyAccount (volumeAccessParty a')) $ \t ->
        createNotification (blankNotification t NoticeVolumeAccess)
          { notificationVolume = Just $ volumeRow v
          , notificationPermission = Just $ volumeAccessIndividual a'
          }
  case api of
    JSON -> return $ okResponse [] $ JSON.objectEncoding $ volumeAccessPartyJSON (if r then a' else a)
    HTML -> peeks $ otherRouteResponse [] viewVolumeAccess arg
