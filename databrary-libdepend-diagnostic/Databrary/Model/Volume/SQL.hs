{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Databrary.Model.Volume.SQL
  ( selectVolumeRow
  , setCreation
  , makeVolume -- TODO: move to Types
  , selectPermissionVolume
  , selectVolume
  , updateVolume
  , insertVolume
  ) where

import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Language.Haskell.TH as TH

import Databrary.Model.Time
import Databrary.Model.SQL.Select
import Databrary.Model.Id.Types
import Databrary.Model.Permission.Types
import Databrary.Model.Audit.SQL
import Databrary.Model.Volume.Types

parseOwner :: T.Text -> VolumeOwner
parseOwner t = (Id $ read $ T.unpack i, T.tail n) where
  (i, n) = T.breakOn ":" t

setCreation :: VolumeRow -> Maybe Timestamp -> [VolumeOwner] -> Permission -> Volume
setCreation r = Volume r . fromMaybe (volumeCreation blankVolume)

makeVolume :: ([VolumeOwner] -> Permission -> a) -> Maybe [Maybe T.Text] -> Maybe Permission -> a
makeVolume vol own perm = vol (maybe [] (map (parseOwner . fromMaybe (error "NULL volume.owner"))) own) (fromMaybe PermissionNONE perm)

selectVolumeRow :: Selector -- ^ @'VolumeRow'@
selectVolumeRow = selectColumns 'VolumeRow "volume" ["id", "name", "body", "alias", "doi"]

selectPermissionVolume :: Selector -- ^ @'Permission' -> 'Volume'@
selectPermissionVolume =
  Selector {
    selectOutput =
       OutputJoin
         False
         'setCreation
         ((selectOutput selectVolumeRow)
          : [SelectExpr "volume_creation(volume.id)"]) -- XXX explicit table references (throughout)
  , selectSource = "volume"
  , selectJoined = ",volume"
  }

selectVolume :: TH.Name -- ^ @'Identity'@
  -> Selector -- ^ @'Volume'@
selectVolume i = selectJoin 'makeVolume
  [ selectPermissionVolume
  , maybeJoinOn "volume.id = volume_owners.volume"
    $ selectColumn "volume_owners" "owners"
  , joinOn "volume_permission.permission >= 'PUBLIC'::permission"
    $ selector ("LATERAL (VALUES (CASE WHEN ${identitySuperuser " ++ is ++ "} THEN enum_last(NULL::permission) ELSE volume_access_check(volume.id, ${view " ++ is ++ " :: Id Party}) END)) AS volume_permission (permission)")
    $ SelectColumn "volume_permission" "permission"
  ]
  where is = nameRef i

volumeKeys :: String -- ^ @'Volume'@
  -> [(String, String)]
volumeKeys v =
  [ ("id", "${volumeId $ volumeRow " ++ v ++ "}") ]

volumeSets :: String -- ^ @'Volume@
  -> [(String, String)]
volumeSets v =
  [ ("name",  "${volumeName $ volumeRow "  ++ v ++ "}")
  , ("alias", "${volumeAlias $ volumeRow " ++ v ++ "}")
  , ("body",  "${volumeBody $ volumeRow "  ++ v ++ "}")
  ]

updateVolume :: TH.Name -- ^ @'AuditIdentity'
  -> TH.Name -- ^ @'Volume'@
  -> TH.ExpQ -- ()
updateVolume ident v = auditUpdate ident "volume"
  (volumeSets vs)
  (whereEq $ volumeKeys vs)
  Nothing
  where vs = nameRef v

insertVolume :: TH.Name -- ^ @'AuditIdentity'
  -> TH.Name -- ^ @'Volume'@
  -> TH.ExpQ -- ^ @'Permission' -> 'Volume'@
insertVolume ident v = auditInsert ident "!volume"
  (volumeSets vs)
  (Just $ selectOutput selectPermissionVolume)
  where vs = nameRef v

