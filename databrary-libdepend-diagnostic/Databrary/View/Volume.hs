{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Databrary.View.Volume
  ( htmlVolumeViewLink
  , htmlVolumeView
  , htmlVolumeEdit
  , htmlVolumeLinksEdit
  , htmlVolumeSearch
  ) where

import Control.Monad (when, forM_)
import Data.Monoid ((<>))
import Network.HTTP.Types.QueryLike (QueryLike(..))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as HA

import Databrary.Has (view)
import Databrary.Action
import Databrary.Model.Permission
import Databrary.Model.Volume
import Databrary.Model.Citation
import Databrary.Model.Tag
import Databrary.HTTP.Form.View
import Databrary.Controller.Paths
import Databrary.View.Html
import Databrary.View.Template
import Databrary.View.Form
import Databrary.View.Paginate

import {-# SOURCE #-} Databrary.Controller.Angular
import {-# SOURCE #-} Databrary.Controller.Party
import {-# SOURCE #-} Databrary.Controller.Volume

htmlVolumeViewLink :: QueryLike q => VolumeRow -> q -> H.Html
htmlVolumeViewLink v q =
  H.a H.! actionLink viewVolume (HTML, volumeId v) q
    $ H.text $ volumeName v

htmlVolumeView :: Volume -> [Tag] -> RequestContext -> H.Html
htmlVolumeView v t req = htmlTemplate req Nothing $ \js -> do
  H.div H.! H.customAttribute "typeof" "dataset" $ do
    H.h1 H.! H.customAttribute "property" "name" $ H.text $ volumeName $ volumeRow v
    when (view v >= PermissionEDIT) $
      H.p $
        H.a H.! actionLink viewVolumeEdit (volumeId $ volumeRow v) js $ "edit"
    H.img
      H.! HA.src (builderValue $ actionURL Nothing thumbVolume (volumeId $ volumeRow v) [])
    H.dl $ do
      forM_ (getVolumeAlias v) $ \a -> do
        H.dt "alias"
        H.dd H.! H.customAttribute "property" "alternateName" $ H.text a
      forM_ (volumeOwners v) $ \(p, n) -> do
        H.dt "owner"
        H.dd H.! H.customAttribute "property" "creator" $ H.a H.! actionLink viewParty (HTML, TargetParty p) js $ H.text n
      forM_ (volumeBody $ volumeRow v) $ \b -> do
        H.dt "body"
        H.dd H.! H.customAttribute "property" "description" $ H.text b -- format
      forM_ (volumeDOI $ volumeRow v) $ \d -> do
        H.dt "doi"
        H.dd H.! H.customAttribute "property" "alternateName" $ byteStringHtml d
      H.dt "keywords"
      H.dd $ H.ul H.! HA.class_ "comma" H.! H.customAttribute "property" "keywords" $ do
        forM_ t $ \n  -> do
          H.li $ byteStringHtml $ tagNameBS $ tagName n

htmlVolumeForm :: Maybe Volume -> Maybe Citation -> FormHtml f
htmlVolumeForm vol cite = do
  field "name" $ inputText $ volumeName . volumeRow <$> vol
  field "alias" $ inputText $ volumeAlias . volumeRow =<< vol
  field "body" $ inputTextarea $ volumeBody . volumeRow =<< vol
  "citation" .:> do
    field "head" $ inputText $ citationHead <$> cite
    field "url" $ inputText $ fmap show $ citationURL =<< cite
    field "year" $ inputText $ fmap show $ citationYear =<< cite

htmlVolumeEdit :: Maybe (Volume, Maybe Citation) -> RequestContext -> FormHtml f
htmlVolumeEdit Nothing = htmlForm "Create volume" createVolume HTML (htmlVolumeForm Nothing Nothing) (const mempty)
htmlVolumeEdit (Just (v, cite)) = htmlForm ("Edit " <> volumeName (volumeRow v)) postVolume (HTML, volumeId $ volumeRow v) (htmlVolumeForm (Just v) cite) (const mempty)

htmlVolumeLinksEdit :: Volume -> [Citation] -> RequestContext -> FormHtml f
htmlVolumeLinksEdit vol links = htmlForm "Edit volume links" postVolumeLinks (HTML, volumeId $ volumeRow vol)
  (withSubFormsViews links $ \link -> do
    field "head" $ inputText $ citationHead <$> link
    field "url" $ inputText $ fmap show $ citationURL =<< link)
  (const mempty)

htmlVolumeList :: JSOpt -> [Volume] -> H.Html
htmlVolumeList js vl = H.ul
  H.! HA.class_ "flat"
  $ forM_ vl $ \v -> H.li
    $ H.article
      H.! HA.class_ "volume-list-result cf"
      $ do
        H.h1
          $ htmlVolumeViewLink (volumeRow v) js
        H.ul
          H.! HA.class_ "flat semicolon"
          $ forM_ (volumeOwners v) $ \(p, o) -> H.li $ do
            H.a H.! actionLink viewParty (HTML, TargetParty p) js
              $ H.text o
        mapM_ (H.p . H.text) $ volumeBody $ volumeRow v

htmlVolumeSearch :: VolumeFilter -> [Volume] -> RequestContext -> FormHtml f
htmlVolumeSearch VolumeFilter{..} vl req = htmlForm "Volume search" queryVolumes HTML
  (field "query" $ inputText volumeFilterQuery)
  (\js -> htmlPaginate (htmlVolumeList js) volumeFilterPaginate vl (view req))
  req
