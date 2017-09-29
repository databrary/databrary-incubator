{-# LANGUAGE TemplateHaskell, QuasiQuotes, RecordWildCards, OverloadedStrings, DataKinds #-}
module Databrary.Model.Tag
  ( module Databrary.Model.Tag.Types
  , lookupTag
  , lookupTags
  , findTags
  , addTag
  , lookupVolumeTagUseRows
  , addTagUse
  , removeTagUse
  , lookupTopTagWeight
  , lookupTagCoverage
  , lookupSlotTagCoverage
  , lookupSlotKeywords
  , tagWeightJSON
  , tagCoverageJSON
  ) where

import Control.Monad (guard)
import qualified Data.ByteString.Char8 as BSC
import Data.Int (Int64)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Database.PostgreSQL.Typed (pgSQL)
import Database.PostgreSQL.Typed.Query (makePGQuery, QueryFlags(..), simpleQueryFlags)

import Databrary.Ops
import Databrary.Has (peek)
import qualified Databrary.JSON as JSON
import Databrary.Service.DB
import Databrary.Model.SQL
import Databrary.Model.SQL.Select
import Databrary.Model.Party.Types
import Databrary.Model.Identity.Types
import Databrary.Model.Volume.Types
import Databrary.Model.Container.Types
import Databrary.Model.Slot.Types
import Databrary.Model.Tag.Types
import Databrary.Model.Tag.SQL

$(useTDB)

lookupTag :: MonadDB c m => TagName -> m (Maybe Tag)
lookupTag n = do
  mRow <- dbQuery1
      $(makePGQuery
          (simpleQueryFlags { flagPrepare = Just [] })
          (   "SELECT tag.id,tag.name"
           ++ " FROM tag " 
           ++ "WHERE tag.name = ${n}::varchar"))
  pure (fmap (\(tid,name) -> Tag tid name) mRow)

lookupTags :: MonadDB c m => m [Tag]
lookupTags = do
  rows <- dbQuery
      $(makePGQuery
          (simpleQueryFlags)
          (   "SELECT tag.id,tag.name"
           ++ " FROM tag " 
           ++ ""))
  pure (fmap (\(tid,name) -> Tag tid name) rows)

findTags :: MonadDB c m => TagName -> Int -> m [Tag]
findTags (TagName n) lim = do -- TagName restrictions obviate pattern escaping
  rows <- dbQuery
      $(makePGQuery
          (simpleQueryFlags { flagPrepare = Just [] })
          (   "SELECT tag.id,tag.name"
           ++ " FROM tag " 
           ++ "WHERE tag.name LIKE ${n `BSC.snoc` '%'}::varchar LIMIT ${fromIntegral lim :: Int64}"))
  pure (fmap (\(tid,name) -> Tag tid name) rows)

addTag :: MonadDB c m => TagName -> m Tag
addTag n =
  dbQuery1' $ (`Tag` n) <$> [pgSQL|!SELECT get_tag(${n})|]

lookupVolumeTagUseRows :: MonadDB c m => Volume -> m [TagUseRow]
lookupVolumeTagUseRows v = do
  rows <- dbQuery
      $(makePGQuery
          (simpleQueryFlags)
          (   "SELECT tag_use.who,tag_use.container,tag_use.segment,tag_use.tableoid = 'keyword_use'::regclass,tag.id,tag.name"
           ++ " FROM tag_use JOIN tag ON tag_use.tag = tag.id " 
           ++ "JOIN container ON tag_use.container = container.id WHERE container.volume = ${volumeId $ volumeRow v} ORDER BY container.id"))
  pure 
    (fmap 
       (\(wh, cn, sg, k, tid, nm) -> 
          ($)
            (($) (makeTagUseRow wh cn sg k))
            (Tag tid nm))
       rows)

addTagUse :: MonadDB c m => TagUse -> m Bool
addTagUse t = either (const False) id <$> do
  dbTryJust (guard . isExclusionViolation)
    $ dbExecute1 (if tagKeyword t
      then $(insertTagUse True 't)
      else $(insertTagUse False 't))

removeTagUse :: MonadDB c m => TagUse -> m Int
removeTagUse t =
  dbExecute
    (if tagKeyword t
      then $(deleteTagUse True 't)
      else $(deleteTagUse False 't))

lookupTopTagWeight :: MonadDB c m => Int -> m [TagWeight]
lookupTopTagWeight lim =
  dbQuery $(selectQuery (selectTagWeight "") "$!ORDER BY weight DESC LIMIT ${fromIntegral lim :: Int64}")

emptyTagCoverage :: Tag -> Container -> TagCoverage
emptyTagCoverage t c = TagCoverage (TagWeight t 0) c [] [] []

lookupTagCoverage :: (MonadDB c m, MonadHasIdentity c m) => Tag -> Slot -> m TagCoverage
lookupTagCoverage t (Slot c s) = do
  ident <- peek
  fromMaybe (emptyTagCoverage t c) <$> dbQuery1 (($ c) . ($ t) <$> $(selectQuery (selectTagCoverage 'ident "WHERE container = ${containerId $ containerRow c} AND segment && ${s} AND tag = ${tagId t}") "$!"))

lookupSlotTagCoverage :: (MonadDB c m, MonadHasIdentity c m) => Slot -> Int -> m [TagCoverage]
lookupSlotTagCoverage slot lim = do
  ident <- peek
  dbQuery $(selectQuery (selectSlotTagCoverage 'ident 'slot) "$!ORDER BY weight DESC LIMIT ${fromIntegral lim :: Int64}")

lookupSlotKeywords :: (MonadDB c m) => Slot -> m [Tag]
lookupSlotKeywords Slot{..} = do
  rows <- dbQuery
      $(makePGQuery
          (simpleQueryFlags)
          (   "SELECT tag.id,tag.name"
           ++ " FROM tag " 
           ++ "JOIN keyword_use ON id = tag WHERE container = ${containerId $ containerRow slotContainer} AND segment = ${slotSegment}"))
  pure (fmap (\(tid,name) -> Tag tid name) rows)

tagWeightJSON :: JSON.ToObject o => TagWeight -> JSON.Record TagName o
tagWeightJSON TagWeight{..} = JSON.Record (tagName tagWeightTag) $
  "weight" JSON..= tagWeightWeight

tagCoverageJSON :: JSON.ToObject o => TagCoverage -> JSON.Record TagName o
tagCoverageJSON TagCoverage{..} = tagWeightJSON tagCoverageWeight JSON..<>
     "coverage" JSON..= tagCoverageSegments
  <> "keyword" JSON..=? (tagCoverageKeywords <!? null tagCoverageKeywords)
  <> "vote"    JSON..=? (tagCoverageVotes    <!? null tagCoverageVotes)
