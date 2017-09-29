{-# LANGUAGE TemplateHaskell, QuasiQuotes, RecordWildCards, DataKinds #-}
module Databrary.Model.VolumeState
  ( module Databrary.Model.VolumeState.Types
  , lookupVolumeState
  , changeVolumeState
  , removeVolumeState
  ) where

import Control.Monad (void)
import Database.PostgreSQL.Typed.Query (pgSQL)
import Database.PostgreSQL.Typed.Query (makePGQuery, QueryFlags(..), simpleQueryFlags)

import Databrary.Service.DB
import Databrary.Model.SQL
import Databrary.Model.Permission.Types
import Databrary.Model.Volume.Types
import Databrary.Model.VolumeState.Types

$(useTDB)

lookupVolumeState :: (MonadDB c m) => Volume -> m [VolumeState]
lookupVolumeState v = do
  rows <- dbQuery
      $(makePGQuery
          (simpleQueryFlags { flagPrepare = Just [] })
          (   "SELECT volume_state.key,volume_state.value,volume_state.public"
           ++ " FROM volume_state " 
           ++ "WHERE volume = ${volumeId $ volumeRow v} AND (public OR ${volumePermission v >= PermissionEDIT})"))
  pure (fmap (\(ky,vl,pb) -> VolumeState ky vl pb v) rows)

changeVolumeState :: (MonadDB c m) => VolumeState -> m ()
changeVolumeState VolumeState{..} = void $ updateOrInsert
  [pgSQL|UPDATE volume_state SET value = ${volumeStateValue}, public = ${volumeStatePublic} WHERE volume = ${volumeId $ volumeRow stateVolume} AND key = ${volumeStateKey}|]
  [pgSQL|INSERT INTO volume_state (volume, key, value, public) VALUES (${volumeId $ volumeRow stateVolume}, ${volumeStateKey}, ${volumeStateValue}, ${volumeStatePublic})|]

removeVolumeState :: (MonadDB c m) => Volume -> VolumeStateKey -> m Bool
removeVolumeState v k =
  dbExecute1 [pgSQL|DELETE FROM volume_state WHERE volume = ${volumeId $ volumeRow v} AND key = ${k}|]
