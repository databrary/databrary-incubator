{-# LANGUAGE TemplateHaskell, QuasiQuotes, RecordWildCards, OverloadedStrings, DataKinds #-}
module Databrary.Model.Funding
  ( module Databrary.Model.Funding.Types
  , lookupFunder
  , findFunders
  , addFunder
  , lookupVolumeFunding
  , changeVolumeFunding
  , removeVolumeFunder
  , funderJSON
  , fundingJSON
  ) where

import Data.Monoid ((<>))
import qualified Data.Text as T
import Database.PostgreSQL.Typed (pgSQL)
import Database.PostgreSQL.Typed.Query (makePGQuery, QueryFlags(..), simpleQueryFlags)

import qualified Databrary.JSON as JSON
import Databrary.Service.DB
import Databrary.Model.SQL
import Databrary.Model.Id.Types
import Databrary.Model.Volume.Types
import Databrary.Model.Funding.Types
import Databrary.Model.Funding.SQL

$(useTDB)

lookupFunder :: MonadDB c m => Id Funder -> m (Maybe Funder)
lookupFunder fi = do
  mRow <- dbQuery1
      $(makePGQuery
          (simpleQueryFlags { flagPrepare = Just [] })
          (   "SELECT funder.fundref_id,funder.name"
           ++ " FROM funder " 
           ++ "WHERE funder.fundref_id = ${fi}"))
  pure (fmap (\(fid,name) -> Funder fid name) mRow)

findFunders :: MonadDB c m => T.Text -> m [Funder]
findFunders q = do
  rows <- dbQuery
      $(makePGQuery
          (simpleQueryFlags { flagPrepare = Just [] })
          (   "SELECT funder.fundref_id,funder.name"
           ++ " FROM funder " 
           ++ "WHERE funder.name ILIKE '%' || ${q} || '%'"))
  pure (fmap (\(fid,name) -> Funder fid name) rows)

addFunder :: MonadDB c m => Funder -> m ()
addFunder f =
  dbExecute1' [pgSQL|INSERT INTO funder (fundref_id, name) VALUES (${funderId f}, ${funderName f})|]

lookupVolumeFunding :: (MonadDB c m) => Volume -> m [Funding]
lookupVolumeFunding vol = do
  rows <- dbQuery
      $(makePGQuery
          (simpleQueryFlags { flagPrepare = Just [] })
          (   "SELECT volume_funding.awards,funder.fundref_id,funder.name "
           ++ " FROM volume_funding JOIN funder ON volume_funding.funder = funder.fundref_id "
           ++ "WHERE volume_funding.volume = ${volumeId $ volumeRow vol}"))
  pure (fmap (\(aws, fid, nm) -> ($) (makeFunding aws) (Funder fid nm)) rows)

changeVolumeFunding :: MonadDB c m => Volume -> Funding -> m Bool
changeVolumeFunding v Funding{..} =
  (0 <) . fst <$> updateOrInsert
    [pgSQL|UPDATE volume_funding SET awards = ${a} WHERE volume = ${volumeId $ volumeRow v} AND funder = ${funderId fundingFunder}|]
    [pgSQL|INSERT INTO volume_funding (volume, funder, awards) VALUES (${volumeId $ volumeRow v}, ${funderId fundingFunder}, ${a})|]
  where a = map Just fundingAwards

removeVolumeFunder :: MonadDB c m => Volume -> Id Funder -> m Bool
removeVolumeFunder v f =
  dbExecute1 [pgSQL|DELETE FROM volume_funding WHERE volume = ${volumeId $ volumeRow v} AND funder = ${f}|]

funderJSON :: JSON.ToObject o => Funder -> o
funderJSON Funder{..} =
     "id" JSON..= funderId
  <> "name" JSON..= funderName

fundingJSON :: JSON.ToNestedObject o u => Funding -> o
fundingJSON Funding{..} =
     "funder" JSON..=. funderJSON fundingFunder
  <> "awards" JSON..= fundingAwards
