{-# LANGUAGE OverloadedStrings #-}
module Databrary.Controller.CSV
  ( csvResponse
  , csvVolume
  , volumeCSV
  ) where

import Control.Arrow (second)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.Foldable (fold)
import Data.Function (on)
import Data.List (foldl', nubBy, groupBy)
import Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.HTTP.Types (hContentType)

import Databrary.Model.Id
import Databrary.Model.Permission
import Databrary.Model.Volume
import Databrary.Model.Container
import Databrary.Model.Record
import Databrary.Model.Category
import Databrary.Model.RecordSlot
import Databrary.Model.Metric
import Databrary.Model.Measure
import Databrary.Model.VolumeMetric
import Databrary.Store.Filename
import Databrary.Store.CSV
import Databrary.HTTP
import Databrary.HTTP.Path.Parser
import Databrary.Action
import Databrary.Controller.Paths
import Databrary.Controller.Volume

csvResponse :: [[BS.ByteString]] -> BS.ByteString -> Response
csvResponse csv save = okResponse
  [ (hContentType, "text/csv;charset=utf-8")
  , ("content-disposition", "attachment; filename=" <> quoteHTTP (save <> ".csv"))
  ] $ buildCSV csv

type Records = [[Record]]
type Metrics = [[Metric]]
type Header = (Category, Metrics)
type Headers = [Header]

tshow :: Show a => a -> BS.ByteString
tshow = BSC.pack . show

tmaybe :: (a -> BS.ByteString) -> Maybe a -> BS.ByteString
tmaybe = maybe BS.empty

tenc :: T.Text -> BS.ByteString
tenc = TE.encodeUtf8

updateHeaders :: [(Category, Int)] -> Records -> [(Category, Int)]
updateHeaders h [] = h
updateHeaders [] l = map (\rl@(r:_) -> (recordCategory $ recordRow r, length rl)) l
updateHeaders hl@(cm@(c,m):hl') rll@(~rl@(r:_):rll') = case compare c rc of
  LT -> cm                     : updateHeaders hl' rll
  EQ -> (c, m `max` length rl) : updateHeaders hl' rll'
  GT -> (rc, length rl)        : updateHeaders hl rll'
  where rc = recordCategory $ recordRow r

metricHeader :: [Metric] -> [BS.ByteString]
metricHeader = map (tenc . metricName)

metricsHeader :: BS.ByteString -> Metrics -> [BS.ByteString]
metricsHeader p [m] = map (BSC.snoc p '-' <>) $ metricHeader m
metricsHeader p ml = mh 0 ml where
  mh _ [] = []
  mh i (m:l) = map (p' <>) (metricHeader m) ++ mh i' l where
    p' = p <> BSC.snoc (tshow i') '-'
    i' = succ i :: Integer

headerRow :: Headers -> [BS.ByteString]
headerRow = concatMap $ uncurry $ metricsHeader . tenc . categoryName

metricsRow :: [Metric] -> [Measure] -> [BS.ByteString]
metricsRow mh@(m:h) dl@(d:l) = case compare m dm of
  LT -> fold (metricAssumed m) : metricsRow h dl
  EQ -> measureDatum d : metricsRow h l
  GT -> metricsRow mh l
  where dm = measureMetric d
metricsRow m _ = map (fold . metricAssumed) m

recordsRow :: Metrics -> [Record] -> [BS.ByteString]
recordsRow h [] = concatMap (`metricsRow` []) h
recordsRow ~(h:hl) (r:rl) = metricsRow h (recordMeasures r) ++ recordsRow hl rl

dataRow :: Headers -> Records -> [BS.ByteString]
dataRow hl@((c,m):hl') rll@(~rl@(r:_):rll') = case compare c rc of
  LT -> recordsRow m [] ++ dataRow hl' rll
  EQ -> recordsRow m rl ++ dataRow hl' rll'
  GT -> dataRow hl rll'
  where rc = recordCategory $ recordRow r
dataRow _ _ = []

volumeCSV :: Volume -> [(Container, [RecordSlot])] -> ActionM [[BS.ByteString]]
volumeCSV vol crsl = do
  mets <- map getMetric' <$> lookupVolumeMetrics vol
  -- FIXME if volume metrics can be reordered
  let grm r = r{ recordMeasures = getRecordMeasures r }
      crl = map (second $ map (nubBy ((==) `on` recordId . recordRow)) . groupBy ((==) `on` recordCategory . recordRow) . map (grm . slotRecord)) crsl
      hl = map (\(c, n) -> (c, replicate n $ filter ((c ==) . metricCategory) mets)) $
        foldl' updateHeaders [] $ map snd crl
      cr c r = tshow (containerId $ containerRow c) : tmaybe tenc (containerName $ containerRow c) : maybe (if containerTop (containerRow c) then "materials" else BS.empty) BSC.pack (formatContainerDate c) : tmaybe tshow (containerRelease c) : dataRow hl r
      hr = "session-id" : "session-name" : "session-date" : "session-release" : headerRow hl
  return $ hr : map (uncurry cr) crl

csvVolume :: ActionRoute (Id Volume)
csvVolume = action GET (pathId </< "csv") $ \vi -> withAuth $ do
  vol <- getVolume PermissionPUBLIC vi
  _:r <- lookupVolumeContainersRecords vol
  csv <- volumeCSV vol r
  return $ csvResponse csv (makeFilename (volumeDownloadName vol))
