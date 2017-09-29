{-# LANGUAGE OverloadedStrings #-}
module Databrary.Controller.Party
  ( getParty
  , viewParty
  , viewPartyEdit
  , viewPartyCreate
  , viewPartyDelete
  , postParty
  , createParty
  , deleteParty
  , viewAvatar
  , queryParties
  , adminParties
  , csvParties
  ) where

import Control.Applicative (optional)
import Control.Monad (unless, when, forM)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import qualified Data.Text.Encoding as TE
import Network.HTTP.Types (badRequest400)
import qualified Network.Wai as Wai
import Network.Wai.Parse (FileInfo(..))

import Databrary.Ops
import Databrary.Has
import qualified Databrary.JSON as JSON
import Databrary.Model.Enum
import Databrary.Model.Id
import Databrary.Model.Permission
import Databrary.Model.Release
import Databrary.Model.Party
import Databrary.Model.ORCID
import Databrary.Model.Authorize
import Databrary.Model.Volume
import Databrary.Model.VolumeAccess
import Databrary.Model.Asset
import Databrary.Model.AssetSlot
import Databrary.Model.AssetSegment
import Databrary.Model.Format
import Databrary.Model.Notification (Notice(NoticeNewsletter), lookupNoticePartyAuthorization)
import Databrary.Store.Temp
import Databrary.HTTP.Path.Parser
import Databrary.HTTP.Form.Deform
import Databrary.Action.Route
import Databrary.Action.Types
import Databrary.Action.Run
import Databrary.Action
import Databrary.Controller.Paths
import Databrary.Controller.Permission
import Databrary.Controller.Form
import Databrary.Controller.Angular
import Databrary.Controller.AssetSegment
import Databrary.Controller.Web
import Databrary.Controller.CSV
import Databrary.View.Party

getParty :: Maybe Permission -> PartyTarget -> ActionM Party
getParty (Just p) (TargetParty i) =
  checkPermission p =<< maybeAction =<< lookupAuthParty i
getParty _ mi = do
  u <- accountParty <$> authAccount
  let isme TargetProfile = True
      isme (TargetParty i) = partyId (partyRow u) == i
  unless (isme mi) $ result =<< peeks forbiddenResponse
  return u

partyJSONField :: Party -> BS.ByteString -> Maybe BS.ByteString -> ActionM (Maybe JSON.Encoding)
partyJSONField p "parents" o = do
  now <- peek
  fmap (Just . JSON.mapObjects id) . mapM (\a -> do
    let ap = authorizeParent (authorization a)
    acc <- if auth && authorizeActive a now then Just . accessSite <$> lookupAuthorization ap rootParty else return Nothing
    return $ (if admin then authorizeJSON a else mempty)
      <> "party" JSON..=: (partyJSON ap JSON..<> "authorization" JSON..=? acc)
      <> "expired" JSON..=? (True <? admin && authorizeExpired a now))
    =<< lookupAuthorizedParents p (admin ?!> PermissionNONE)
  where
  admin = view p >= PermissionADMIN
  auth = admin && o == Just "authorization"
partyJSONField p "children" _ =
  Just . JSON.mapObjects (\a ->
    let ap = authorizeChild (authorization a) in
    (if admin then authorizeJSON a else mempty) <> "party" JSON..=: partyJSON ap)
    <$> lookupAuthorizedChildren p (admin ?!> PermissionNONE)
  where admin = view p >= PermissionADMIN
partyJSONField p "volumes" o = (?$>) (view p >= PermissionADMIN) $
  fmap (JSON.mapRecords id) . mapM vf =<< lookupPartyVolumes p PermissionREAD
  where
  vf v
    | o == Just "access" = do
      a <- lookupVolumeAccess v (succ PermissionNONE)
      return $ volumeJSON v JSON..<> JSON.nestObject "access" (\u -> map (u . volumeAccessPartyJSON) a)
    | otherwise = return $ volumeJSON v
partyJSONField p "access" ma = do
  Just . JSON.mapObjects volumeAccessVolumeJSON
    <$> lookupPartyVolumeAccess p (fromMaybe PermissionEDIT $ readDBEnum . BSC.unpack =<< ma)
partyJSONField p "authorization" _ = do
  Just . JSON.toEncoding . accessSite <$> lookupAuthorization p rootParty
partyJSONField _ _ _ = return Nothing

partyJSONQuery :: Party -> JSON.Query -> ActionM (JSON.Record (Id Party) JSON.Series)
partyJSONQuery p q = (partyJSON p JSON..<>) <$> JSON.jsonQuery (partyJSONField p) q

viewParty :: ActionRoute (API, PartyTarget)
viewParty = action GET (pathAPI </> pathPartyTarget) $ \(api, i) -> withAuth $ do
  when (api == HTML) angular
  p <- getParty (Just PermissionNONE) i
  case api of
    JSON -> okResponse [] <$> (partyJSONQuery p =<< peeks Wai.queryString)
    HTML -> peeks $ okResponse [] . htmlPartyView p

processParty :: API -> Maybe Party -> ActionM (Party, Maybe (Maybe Asset))
processParty api p = do
  (p', a) <- runFormFiles [("avatar", maxAvatarSize)] (api == HTML ?> htmlPartyEdit p) $ do
    csrfForm
    name <- "sortname" .:> (deformRequired =<< deform)
    prename <- "prename" .:> deformNonEmpty deform
    orcid <- "orcid" .:> deformNonEmpty (deformRead blankORCID)
    affiliation <- "affiliation" .:> deformNonEmpty deform
    url <- "url" .:> deformNonEmpty deform
    avatar <- "avatar" .:>
      (maybe (deformOptional $ return Nothing) (\a -> do
        f <- deformCheck "Must be an image." formatIsImage =<<
          deformMaybe' "Unknown or unsupported file format."
          (getFormatByFilename (fileName a))
        return $ Just $ Just (a, f)) =<< deform)
    return (bp
      { partyRow = (partyRow bp)
        { partySortName = name
        , partyPreName = prename
        , partyORCID = orcid
        , partyAffiliation = affiliation
        , partyURL = url
        }
      }, avatar)
  a' <- forM a $ mapM $ \(af, fmt) -> do
    let ba = blankAsset coreVolume
    a' <- addAsset ba
      { assetRow = (assetRow ba)
        { assetFormat = fmt
        , assetRelease = Just ReleasePUBLIC
        , assetName = Just $ TE.decodeUtf8 $ fileName af
        }
      } $ Just $ tempFilePath (fileContent af)
    focusIO $ releaseTempFile $ fileContent af
    return a'
  return (p', a')
  where
  maxAvatarSize = 10*1024*1024
  bp = fromMaybe blankParty p

viewPartyEdit :: ActionRoute PartyTarget
viewPartyEdit = action GET (pathHTML >/> pathPartyTarget </< "edit") $ \i -> withAuth $ do
  angular
  p <- getParty (Just PermissionEDIT) i
  peeks $ blankForm . htmlPartyEdit (Just p)

viewPartyCreate :: ActionRoute ()
viewPartyCreate = action GET (pathHTML </< "party" </< "create") $ \() -> withAuth $ do
  checkMemberADMIN
  peeks $ blankForm . htmlPartyEdit Nothing

postParty :: ActionRoute (API, PartyTarget)
postParty = multipartAction $ action POST (pathAPI </> pathPartyTarget) $ \(api, i) -> withAuth $ do
  p <- getParty (Just PermissionEDIT) i
  (p', a) <- processParty api (Just p)
  changeParty p'
  mapM_ (changeAvatar p') a
  case api of
    JSON -> return $ okResponse [] $ JSON.recordEncoding $ partyJSON p'
    HTML -> peeks $ otherRouteResponse [] viewParty (api, i)

createParty :: ActionRoute API
createParty = multipartAction $ action POST (pathAPI </< "party") $ \api -> withAuth $ do
  checkMemberADMIN
  (bp, a) <- processParty api Nothing
  p <- addParty bp
  mapM_ (changeAvatar p) a
  case api of
    JSON -> return $ okResponse [] $ JSON.recordEncoding $ partyJSON p
    HTML -> peeks $ otherRouteResponse [] viewParty (api, TargetParty $ partyId $ partyRow p)

deleteParty :: ActionRoute (Id Party)
deleteParty = action POST (pathHTML >/> pathId </< "delete") $ \i -> withAuth $ do
  checkMemberADMIN
  p <- getParty (Just PermissionADMIN) (TargetParty i)
  r <- removeParty p
  return $ if r
    then okResponse [] $ partyName (partyRow p) <> " deleted"
    else response badRequest400 [] $ partyName (partyRow p) <> " not deleted"

viewPartyDelete :: ActionRoute (Id Party)
viewPartyDelete = action GET (pathHTML >/> pathId </< "delete") $ \i -> withAuth $ do
  checkMemberADMIN
  p <- getParty (Just PermissionADMIN) (TargetParty i)
  peeks $ blankForm . htmlPartyDelete p

viewAvatar :: ActionRoute (Id Party)
viewAvatar = action GET (pathId </< "avatar") $ \i -> withoutAuth $
  maybe
    (peeks $ otherRouteResponse [] webFile (Just $ staticPath ["images", "avatar.png"]))
    (serveAssetSegment False . assetSlotSegment . assetNoSlot)
    =<< lookupAvatar i

partySearchForm :: DeformActionM f PartyFilter
partySearchForm = PartyFilter
  <$> ("query" .:> deformNonEmpty deform)
  <*> ("authorization" .:> optional deform)
  <*> ("institution" .:> deformNonEmpty deform)
  <*> paginateForm

queryParties :: ActionRoute API
queryParties = action GET (pathAPI </< "party") $ \api -> withAuth $ do
  when (api == HTML) angular
  pf <- runForm (api == HTML ?> htmlPartySearch mempty []) partySearchForm
  p <- findParties pf
  case api of
    JSON -> return $ okResponse [] $ JSON.mapRecords partyJSON p
    HTML -> peeks $ blankForm . htmlPartySearch pf p

adminParties :: ActionRoute ()
adminParties = action GET ("party" </< "admin") $ \() -> withAuth $ do
  checkMemberADMIN
  pf <- runForm (Just $ htmlPartyAdmin mempty []) partySearchForm
  p <- findParties pf
  peeks $ blankForm . htmlPartyAdmin pf p

csvParties :: ActionRoute ()
csvParties = action GET ("party" </< "csv") $ \() -> withAuth $ do
  checkMemberADMIN
  pl <- lookupNoticePartyAuthorization NoticeNewsletter
  return $ csvResponse 
    [ [ BSC.pack $ show $ partyId $ partyRow p
      , TE.encodeUtf8 $ partySortName $ partyRow p
      , c TE.encodeUtf8 $ partyPreName $ partyRow p
      , c accountEmail $ partyAccount p
      , c (BSC.pack . show) a
      , BSC.pack $ show $ fromEnum d
      ]
    | (p, a, d) <- pl ] "party"
  where c = maybe BS.empty
