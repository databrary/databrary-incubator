{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Databrary.Model.Container.SQL
  (-- selectContainerRow
    selectVolumeContainer
  , selectContainer
  , insertContainer
  , updateContainer
  , deleteContainer
  ) where

import qualified Language.Haskell.TH as TH

import Databrary.Model.SQL.Select
import Databrary.Model.Id.Types
import Databrary.Model.Audit.SQL
import Databrary.Model.Volume.SQL
import Databrary.Model.Release.SQL
import Databrary.Model.Container.Types

selectVolumeContainer :: Selector -- ^ @'Volume' -> 'Container'@
selectVolumeContainer = selectJoin 'Container
  [ selectColumns 'ContainerRow "container" ["id", "top", "name", "date"]
  , maybeJoinOn "container.id = slot_release.container AND slot_release.segment = '(,)'"
    (selectColumn "slot_release" "release")
  ]

selectContainer :: TH.Name -- ^ @'Identity'@  -- only used by Comment now
  -> Selector -- ^ @'Container'@
selectContainer ident = selectJoin '($)
  [ selectVolumeContainer
  , joinOn "container.volume = volume.id" $ selectVolume ident
  ]

containerKeys :: String -- ^ @'Container'@
  -> [(String, String)]
containerKeys o =
  [ ("id", "${containerId $ containerRow " ++ o ++ "}") ]

containerSets :: String -- ^ @'Container'@
  -> [(String, String)]
containerSets o =
  [ ("volume", "${volumeId $ volumeRow $ containerVolume " ++ o ++ "}")
  , ("top", "${containerTop $ containerRow " ++ o ++ "}")
  , ("name", "${containerName $ containerRow " ++ o ++ "}")
  , ("date", "${containerDate $ containerRow " ++ o ++ "}")
  ]

setContainerId :: Container -> Id Container -> Container
setContainerId o i = o{ containerRow = (containerRow o) { containerId = i } }

insertContainer :: TH.Name -- ^ @'AuditIdentity'@
  -> TH.Name -- ^ @'Container'@
  -> TH.ExpQ -- ^ @'Container'@
insertContainer ident o = auditInsert ident "container"
  (containerSets (nameRef o))
  (Just $ selectOutput $ selectMap ((TH.VarE 'setContainerId `TH.AppE` TH.VarE o) `TH.AppE`) $ selectColumn "container" "id")

updateContainer :: TH.Name -- ^ @'AuditIdentity'@
  -> TH.Name -- ^ @'Container'@
  -> TH.ExpQ -- ^ @()@
updateContainer ident o = auditUpdate ident "container"
  (containerSets (nameRef o))
  (whereEq $ containerKeys (nameRef o))
  Nothing

deleteContainer :: TH.Name -- ^ @'AuditIdentity'@
  -> TH.Name -- ^ @'Container'@
  -> TH.ExpQ -- ^ @()@
deleteContainer ident o = auditDelete ident "container"
  (whereEq $ containerKeys (nameRef o))
  Nothing

