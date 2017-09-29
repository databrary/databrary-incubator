{-# LANGUAGE TemplateHaskell, QuasiQuotes, DataKinds #-}
module Databrary.Model.AssetRevision
  ( module Databrary.Model.AssetRevision.Types
  , replaceAsset
  , assetIsReplaced
  , lookupAssetReplace
  , lookupAssetTranscode
  ) where

import Database.PostgreSQL.Typed.Query (pgSQL)

import Databrary.Has
import Databrary.Service.DB
import Databrary.Model.SQL
import Databrary.Model.Id
import Databrary.Model.Party
import Databrary.Model.Identity
import Databrary.Model.Asset
import Databrary.Model.AssetRevision.Types
import Databrary.Model.AssetRevision.SQL

useTDB

replaceAsset :: MonadDB c m => Asset -> Asset -> m ()
replaceAsset old new =
  dbExecute1' [pgSQL|SELECT asset_replace(${assetId $ assetRow old}, ${assetId $ assetRow new})|]

assetIsReplaced :: MonadDB c m => Asset -> m Bool
assetIsReplaced a =
  dbExecute1 [pgSQL|SELECT ''::void FROM asset_replace WHERE orig = ${assetId $ assetRow a} LIMIT 1|]

lookupAssetReplace :: (MonadHasIdentity c m, MonadDB c m) => Asset -> m (Maybe AssetRevision)
lookupAssetReplace a = do
  ident <- peek
  dbQuery1 $ ($ a) <$> $(selectQuery (selectAssetRevision "asset_replace" 'ident) "$WHERE asset_replace.asset = ${assetId $ assetRow a}")

lookupAssetTranscode :: (MonadHasIdentity c m, MonadDB c m) => Asset -> m (Maybe AssetRevision)
lookupAssetTranscode a = do
  ident <- peek
  dbQuery1 $ ($ a) <$> $(selectQuery (selectAssetRevision "transcode" 'ident) "$WHERE transcode.asset = ${assetId $ assetRow a}")
