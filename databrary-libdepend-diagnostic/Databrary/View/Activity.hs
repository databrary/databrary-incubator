{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Databrary.View.Activity
  ( htmlSiteActivity
  ) where

import qualified Data.Array.Unboxed as A
import Data.List (find)
import qualified Data.Map.Strict as M
import qualified Text.Blaze.Html5 as H

import Databrary.Model.Stats.Types
import Databrary.Model.Permission.Types
import Databrary.Model.Category
import Databrary.Action.Types
import Databrary.View.Template

htmlSiteActivity :: SiteStats -> RequestContext -> H.Html
htmlSiteActivity SiteStats{..} req = htmlTemplate req (Just "activity") $ \_ -> do
  H.ul $ do
    H.li $ sh (statsAuthorizedSite A.! PermissionEDIT) >> " authorized PIs"
    H.li $ sh (statsAuthorizedSite A.! PermissionREAD) >> " affiliates who have access to the shared data"
    H.li $ sh (statsAuthorizedSite A.! PermissionADMIN) >> " universities"
    H.li $ sh statsVolumes >> " volumes, " >> sh statsVolumesShared >> " have been opened for sharing"
    H.li $ sh statsAssetDuration >> " of recordings and " >> sh statsAssets >> " files from " >> sh (maybe 0 ((statsRecords M.!) . categoryId) participant) >> " participants"
  return ()
  where
  participant = find (("participant" ==) . categoryName) allCategories
  sh :: Show a => a -> H.Markup
  sh = H.string . show
