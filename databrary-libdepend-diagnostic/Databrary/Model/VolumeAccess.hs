{-# LANGUAGE TemplateHaskell, RecordWildCards, OverloadedStrings, ScopedTypeVariables, DataKinds #-}
module Databrary.Model.VolumeAccess
  ( module Databrary.Model.VolumeAccess.Types
  , lookupVolumeAccess
  , lookupVolumeAccessParty
  , lookupPartyVolumeAccess
  , lookupPartyVolumes
  , changeVolumeAccess
  , volumeAccessProvidesADMIN
  , volumeAccessJSON
  , volumeAccessPartyJSON
  , volumeAccessVolumeJSON
  , lookupVolumeShareActivity
  ) where

import Data.Int (Int64)
import Data.Monoid ((<>))

import Databrary.Ops
import Databrary.Has (peek, view)
import qualified Databrary.JSON as JSON
import Databrary.Service.DB
import Databrary.Model.SQL
import Databrary.Model.SQL.Select (selectDistinctQuery)
import Databrary.Model.Time
import Databrary.Model.Id.Types
import Databrary.Model.Permission.Types
import Databrary.Model.Identity.Types
import Databrary.Model.Audit
import Databrary.Model.Party
import Databrary.Model.Volume
import Databrary.Model.Volume.SQL
import Databrary.Model.VolumeAccess.Types
import Databrary.Model.VolumeAccess.SQL

lookupVolumeAccess :: (MonadDB c m, MonadHasIdentity c m) => Volume -> Permission -> m [VolumeAccess]
lookupVolumeAccess vol perm = do
  ident <- peek
  dbQuery $(selectQuery (selectVolumeAccess 'vol 'ident) "$WHERE volume_access.individual >= ${perm} ORDER BY individual DESC, sort")

lookupVolumeAccessParty :: (MonadDB c m, MonadHasIdentity c m) => Volume -> Id Party -> m (Maybe VolumeAccess)
lookupVolumeAccessParty vol p = do
  ident <- peek
  dbQuery1 $(selectQuery (selectVolumeAccessParty 'vol 'ident) "WHERE party.id = ${p}")

lookupPartyVolumeAccess :: (MonadDB c m, MonadHasIdentity c m) => Party -> Permission -> m [VolumeAccess]
lookupPartyVolumeAccess p perm = do
  ident <- peek
  dbQuery $(selectQuery (selectPartyVolumeAccess 'p 'ident) "$WHERE volume_access.individual >= ${perm} ORDER BY individual DESC, children DESC")

lookupPartyVolumes :: (MonadDB c m, MonadHasIdentity c m) => Party -> Permission -> m [Volume]
lookupPartyVolumes p perm = do
  ident <- peek
  dbQuery $(selectDistinctQuery (Just ["volume.id"]) (selectVolume 'ident) "$JOIN volume_access_view ON volume.id = volume_access_view.volume WHERE party = ${partyId $ partyRow p} AND access >= ${perm}")

changeVolumeAccess :: (MonadAudit c m) => VolumeAccess -> m Bool
changeVolumeAccess va = do
  ident <- getAuditIdentity
  if volumeAccessIndividual va == PermissionNONE
    then dbExecute1 $(deleteVolumeAccess 'ident 'va)
    else (0 <) . fst <$> updateOrInsert
      $(updateVolumeAccess 'ident 'va)
      $(insertVolumeAccess 'ident 'va)

volumeAccessProvidesADMIN :: VolumeAccess -> Bool
volumeAccessProvidesADMIN VolumeAccess{ volumeAccessChildren   = PermissionADMIN, volumeAccessParty = p } = accessMember     p == PermissionADMIN
volumeAccessProvidesADMIN VolumeAccess{ volumeAccessIndividual = PermissionADMIN, volumeAccessParty = p } = accessPermission p == PermissionADMIN
volumeAccessProvidesADMIN _ = False

volumeAccessJSON :: JSON.ToObject o => VolumeAccess -> o
volumeAccessJSON VolumeAccess{..} =
     "individual" JSON..=? (volumeAccessIndividual <? volumeAccessIndividual >= PermissionNONE)
  <> "children"   JSON..=? (volumeAccessChildren   <? volumeAccessChildren   >= PermissionNONE)
  <> "sort" JSON..=? volumeAccessSort

volumeAccessPartyJSON :: JSON.ToNestedObject o u => VolumeAccess -> o
volumeAccessPartyJSON va@VolumeAccess{..} = volumeAccessJSON va
  <> "party" JSON..=: partyJSON volumeAccessParty

volumeAccessVolumeJSON :: JSON.ToNestedObject o u => VolumeAccess -> o
volumeAccessVolumeJSON va@VolumeAccess{..} = volumeAccessJSON va
  <> "volume" JSON..=: volumeJSON volumeAccessVolume

lookupVolumeShareActivity :: (MonadDB c m, MonadHasIdentity c m) => Int -> m [(Timestamp, Volume)]
lookupVolumeShareActivity limit = do
  ident :: Identity <- peek
  dbQuery $(selectQuery (selectVolumeActivity 'ident) "$WHERE audit.audit_action = 'add' AND audit.party = 0 AND audit.children > 'NONE' ORDER BY audit.audit_time DESC LIMIT ${fromIntegral limit :: Int64}")
