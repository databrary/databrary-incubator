{-# LANGUAGE TemplateHaskell #-}
module Databrary.Model.Tag.SQL
  ( insertTagUse
  , deleteTagUse
  , selectTagWeight
  , selectTagCoverage
  , selectSlotTagCoverage
  , makeTagUseRow -- TODO: move to Types
  ) where

import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Database.PostgreSQL.Typed.Query (makePGQuery, simpleQueryFlags)
import qualified Language.Haskell.TH as TH

import Databrary.Model.SQL.Select
import Databrary.Model.Id.Types
import Databrary.Model.Party.Types
import Databrary.Model.Container.Types
import Databrary.Model.Segment
import Databrary.Model.Slot.Types
import Databrary.Model.Tag.Types

tagUseTable :: Bool -> String
tagUseTable False = "tag_use"
tagUseTable True = "keyword_use"

makeTagUseRow :: Id Party -> Id Container -> Segment -> Maybe Bool -> Tag -> TagUseRow
makeTagUseRow w c s k t = TagUseRow t (fromMaybe False k) w (SlotId c s)

insertTagUse :: Bool -- ^ keyword
  -> TH.Name -- ^ @'TagUse'@
  -> TH.ExpQ
insertTagUse keyword o = makePGQuery simpleQueryFlags $
  "INSERT INTO " ++ tagUseTable keyword ++ " (tag, container, segment, who) VALUES (${tagId $ useTag " ++ os ++ "}, ${containerId $ containerRow $ slotContainer $ tagSlot  " ++ os ++ "}, ${slotSegment $ tagSlot  " ++ os ++ "}, ${partyId $ partyRow $ accountParty $ tagWho  " ++ os ++ "})"
  where os = nameRef o

deleteTagUse :: Bool -- ^ keyword
  -> TH.Name -- ^ @'TagUse'@
  -> TH.ExpQ
deleteTagUse keyword o = makePGQuery simpleQueryFlags $
  "DELETE FROM ONLY " ++ tagUseTable keyword ++ " WHERE tag = ${tagId $ useTag " ++ os ++ "} AND container = ${containerId $ containerRow $ slotContainer $ tagSlot " ++ os ++ "} AND segment <@ ${slotSegment $ tagSlot " ++ os ++ "}"
  ++ (if keyword then "" else " AND who = ${partyId $ partyRow $ accountParty $ tagWho " ++ os ++ "}")
  where os = nameRef o

selectTagGroup :: String -- ^ table name
  -> String -- ^ query
  -> TH.Name -- ^ make function
  -> [(String, String)] -- ^ select columns (alias, select)
  -> Selector
selectTagGroup name q make cols = selector
  ("(SELECT tag," ++ intercalate "," (map (\(a, s) -> s ++ " AS " ++ a) cols)
    ++ " FROM tag_use " ++ q ++ " GROUP BY tag) AS " ++ name)
  $ OutputJoin False make $ map (SelectColumn name . fst) cols

tagWeightColumns :: [(String, String)]
tagWeightColumns =
  [ ("weight", "count(*)::integer")
  ]

makeTagWeight :: Int32 -> Tag -> TagWeight
makeTagWeight w t = TagWeight t w

selectTagWeight :: String -> Selector -- ^ @'TagCoverage'@
selectTagWeight q = selectJoin '($)
  [ selectTagGroup "tag_weight" q 'makeTagWeight tagWeightColumns
  , joinOn "tag_weight.tag = tag.id" (selectColumns 'Tag "tag" ["id", "name"])
  ]

makeTagCoverage :: Int32 -> [Maybe Segment] -> [Maybe Segment] -> [Maybe Segment] -> Tag -> Container -> TagCoverage
makeTagCoverage w s k v t c = TagCoverage (TagWeight t w) c (segs s) (segs k) (segs v) where
  segs = map $ fromMaybe (error "NULL tag segment")

tagCoverageColumns :: TH.Name -- ^ @'Party'@
  -> [(String, String)]
tagCoverageColumns acct = tagWeightColumns ++
  [ ("coverage", "segments_union(segment)")
  , ("keywords", "segments_union(CASE WHEN tableoid = 'keyword_use'::regclass THEN segment ELSE 'empty' END)")
  , ("votes", "segments_union(CASE WHEN tableoid = 'tag_use'::regclass AND who = ${partyId $ partyRow " ++ nameRef acct ++ "} THEN segment ELSE 'empty' END)")
  ]

selectTagCoverage :: TH.Name -- ^ @'Party'@
  -> String -- ^ query
  -> Selector -- ^ @'Tag' -> 'Container' -> 'TagCoverage'@
selectTagCoverage acct q =
  selectTagGroup "tag_coverage" q 'makeTagCoverage $ tagCoverageColumns acct

selectSlotTagCoverage :: TH.Name -- ^ @'Party'@
  -> TH.Name -- ^ @'Slot'
  -> Selector -- ^ @'TagCoverage'@
selectSlotTagCoverage acct slot = selectMap (`TH.AppE` (TH.VarE 'slotContainer `TH.AppE` TH.VarE slot)) $ selectJoin '($)
  [ selectTagCoverage acct $ "WHERE container = ${containerId $ containerRow $ slotContainer " ++ ss ++ "} AND segment && ${slotSegment " ++ ss ++ "}"
  , joinOn "tag_coverage.tag = tag.id" (selectColumns 'Tag "tag" ["id", "name"]) 
  ] where ss = nameRef slot
