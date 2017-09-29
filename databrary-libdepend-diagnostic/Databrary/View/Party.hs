{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Databrary.View.Party
  ( htmlPartyViewLink
  , htmlPartyView
  , htmlPartyEdit
  , htmlPartySearch
  , htmlPartyAdmin
  , htmlPartyDelete
  ) where

import Control.Monad (when, forM_, void)
import qualified Data.ByteString.Char8 as BSC
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Network.HTTP.Types.QueryLike (QueryLike(..))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as HA

import Databrary.Has (view)
import Databrary.Model.Permission
import Databrary.Model.Party
import Databrary.Model.ORCID
import Databrary.Store.Temp
import Databrary.Action.Types
import Databrary.Action
import Databrary.Controller.Paths
import Databrary.View.Html
import Databrary.View.Template
import Databrary.View.Form
import Databrary.View.Paginate

import {-# SOURCE #-} Databrary.Controller.Angular
import {-# SOURCE #-} Databrary.Controller.Party
import {-# SOURCE #-} Databrary.Controller.Volume
import {-# SOURCE #-} Databrary.Controller.Register

htmlPartyViewLink :: QueryLike q => PartyRow -> q -> H.Html
htmlPartyViewLink p q =
  H.a H.! actionLink viewParty (HTML, TargetParty $ partyId p) q
    $ H.text $ partyName p

htmlPartyView :: Party -> RequestContext -> H.Html
htmlPartyView p@Party{ partyRow = pr@PartyRow{..}, ..} req = htmlTemplate req Nothing $ \js -> do
  H.div H.! H.customAttribute "typeof" "person" $ do
    H.h1 H.! H.customAttribute "property" "name" $ H.text $ partyName pr
    when (view p >= PermissionEDIT) $
      H.p $
        H.a H.! actionLink viewPartyEdit (TargetParty partyId) js $ "edit"
    H.img
      H.! HA.src (builderValue $ actionURL Nothing viewAvatar partyId [])
    H.dl $ do
      forM_ partyAffiliation $ \a -> do
        H.dt "affiliation"
        H.dd H.! H.customAttribute "property" "affiliation" $ H.text a
      forM_ partyURL $ \u -> do
        let us = show u
        H.dt "url"
        H.dd H.! H.customAttribute "property" "url" $ H.a H.! HA.href (H.stringValue us) $ H.string us
      forM_ (partyEmail p) $ \e -> do
        H.dt "email"
        H.dd H.! H.customAttribute "property" "email" $ H.a H.! HA.href (byteStringValue $ "mailto:" <> e) $ byteStringHtml e
      forM_ partyORCID $ \o -> do
        H.dt "orcid"
        H.dd H.! H.customAttribute "property" "sameAs" $ H.a H.! HA.href (H.stringValue $ show $ orcidURL o) $ H.string $ show o
    H.a H.! actionLink queryVolumes HTML (toQuery js <> [("party", Just $ BSC.pack $ show partyId)]) $ "volumes"
    return ()

htmlPartyForm :: Maybe Party -> FormHtml TempFile
htmlPartyForm t = do
  field "prename" $ inputText $ partyPreName . partyRow =<< t
  field "sortname" $ inputText $ partySortName . partyRow <$> t
  field "affiliation" $ inputText $ partyAffiliation . partyRow =<< t
  field "url" $ inputText $ show <$> (partyURL . partyRow =<< t)

htmlPartyEdit :: Maybe Party -> RequestContext -> FormHtml TempFile
htmlPartyEdit t = maybe
  (htmlForm "Create party" createParty HTML)
  (\p -> htmlForm
    ("Edit " <> partyName (partyRow p))
    postParty (HTML, TargetParty $ partyId $ partyRow p))
  t
  (htmlPartyForm t)
  (const mempty)

htmlPartyList :: JSOpt -> [Party] -> H.Html
htmlPartyList js pl = H.ul $ forM_ pl $ \p -> H.li $ do
  H.h2 $ htmlPartyViewLink (partyRow p) js
  mapM_ H.text $ partyAffiliation $ partyRow p

htmlPartySearchForm :: PartyFilter -> FormHtml f
htmlPartySearchForm pf = do
  field "query" $ inputText $ partyFilterQuery pf
  field "authorization" $ inputEnum False $ partyFilterAuthorization pf
  field "institution" $ inputCheckbox $ fromMaybe False $ partyFilterInstitution pf

htmlPartySearch :: PartyFilter -> [Party] -> RequestContext -> FormHtml f
htmlPartySearch pf pl req = htmlForm "Search users" queryParties HTML
  (htmlPartySearchForm pf)
  (\js -> htmlPaginate (htmlPartyList js) (partyFilterPaginate pf) pl (view req))
  req

htmlPartyAdmin :: PartyFilter -> [Party] -> RequestContext -> FormHtml f
htmlPartyAdmin pf pl req = htmlForm "party admin" adminParties ()
  (htmlPartySearchForm pf)
  (\js -> htmlPaginate
    (\pl' -> H.table $ do
      H.thead $
        H.tr $ do
          H.th "id"
          H.th "name"
          H.th "email"
          H.th "affiliation"
          H.th "act"
      H.tbody $
        forM_ pl' $ \Party{ partyRow = pr@PartyRow{..}, ..} -> H.tr $ do
          H.td $ H.a H.! actionLink viewParty (HTML, TargetParty partyId) js
            $ H.string $ show partyId
          H.td $ H.text $ partyName pr
          H.td $ mapM_ (byteStringHtml . accountEmail) partyAccount
          H.td $ mapM_ H.text partyAffiliation
          H.td $ do
            actionForm resendInvestigator partyId js
              $ H.input H.! HA.type_ "submit" H.! HA.value "agreement"
            H.a H.! actionLink viewPartyDelete partyId js
              $ "delete"
    )
    (partyFilterPaginate pf) pl (view req))
  req

htmlPartyDelete :: Party -> RequestContext -> FormHtml f
htmlPartyDelete Party{ partyRow = pr@PartyRow{..}, ..} = htmlForm ("delete " <> partyName pr)
  deleteParty partyId
  (return ())
  (void . htmlPartyViewLink pr)
