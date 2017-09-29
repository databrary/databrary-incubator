{-# LANGUAGE TemplateHaskell, QuasiQuotes, DataKinds #-}
module Databrary.Model.Stats
  ( lookupSiteStats
  ) where

import Control.Monad (liftM2)
import qualified Data.Array.Unboxed as A
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Scientific (toBoundedInteger)
import Database.PostgreSQL.Typed.Query (pgSQL)

import Databrary.Service.DB
import Databrary.Model.Stats.Types

useTDB

lookupSiteStats :: MonadDB c m => m SiteStats
lookupSiteStats = do
  ac <- dbQuery [pgSQL|SELECT site, count(child) FROM authorize_view WHERE parent = 0 AND child > 4 GROUP BY site|]
  v <- dbQuery1' [pgSQL|SELECT count(id) FROM volume WHERE id > 0|]
  vs <- dbQuery1' [pgSQL|SELECT count(volume) FROM volume_access WHERE volume > 0 AND party = 0 AND children >= 'PUBLIC'|]
  (a, ad, ab) <- dbQuery1' [pgSQL|SELECT count(id), sum(duration), sum(size) FROM asset JOIN slot_asset ON asset = id WHERE volume > 0|]
  rc <- dbQuery [pgSQL|SELECT category, count(id) FROM record GROUP BY category ORDER BY category|]
  return SiteStats
    { statsAuthorizedSite = A.accumArray (+) 0 (minBound, maxBound) $ l ac
    , statsVolumes = z v
    , statsVolumesShared = z vs
    , statsAssets = z a
    , statsAssetDuration = z ad
    , statsAssetBytes = z $ toBoundedInteger =<< ab
    , statsRecords = M.fromDistinctAscList $ l rc
    }
  where
  z :: Num a => Maybe a -> a
  z = fromMaybe 0
  l :: [(Maybe a, Maybe b)] -> [(a, b)]
  l = mapMaybe (uncurry $ liftM2 (,))
