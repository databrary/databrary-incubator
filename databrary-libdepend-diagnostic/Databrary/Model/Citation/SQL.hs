{-# LANGUAGE TemplateHaskell #-}
module Databrary.Model.Citation.SQL
  (--  selectVolumeCitation
    selectCitation
  , insertVolumeCitation
  , updateVolumeCitation
  , deleteVolumeCitation
  , selectVolumeLink
  , insertVolumeLink
  , deleteVolumeLink
  ) where

import qualified Data.Text as T
import qualified Language.Haskell.TH as TH

import Databrary.Ops
import Databrary.Model.SQL.Select
import Databrary.Model.Audit.SQL
import Databrary.Model.Volume.Types
import Databrary.Model.Volume.SQL
import Databrary.Model.Citation.Types

-- citationRow :: Selector -- ^ @Maybe 'T.Text' -> 'Citation'@
-- citationRow = selectColumns 'Citation "volume_citation" ["head", "url", "year"]

-- selectVolumeCitation :: Selector -- ^ @Maybe 'T.Text' -> 'Citation'@
-- selectVolumeCitation = citationRow

makeVolumeCitation :: Volume -> Maybe (Maybe T.Text -> Citation) -> (Volume, Maybe Citation)
makeVolumeCitation v cf = (v, cf <*- Just (volumeName $ volumeRow v))

selectCitation :: TH.Name -- ^ @'Identity'@
  -> Selector -- ^ @('Volume', Maybe 'Citation')@
selectCitation i = selectJoin 'makeVolumeCitation
  [ selectVolume i
  , maybeJoinOn "volume.id = volume_citation.volume"
    (selectColumns 'Citation "volume_citation" ["head", "url", "year"])
  ]

linkRow :: Selector -- ^ @'Citation'@
linkRow = selectColumns 'Citation "volume_link" ["head", "url"]

selectVolumeLink :: Selector -- ^ @'Citation'@
selectVolumeLink = selectMap ((`TH.AppE` TH.ConE 'Nothing) . (`TH.AppE` TH.ConE 'Nothing))
  linkRow

volumeKeys :: String -- ^ @'Volume'@
  -> [(String, String)]
volumeKeys v =
  [ ("volume", "${volumeId $ volumeRow " ++ v ++ "}") ]

linkSets :: String -- ^ @'Citation'@
  -> [(String, String)]
linkSets c =
  [ ("head", "${citationHead " ++ c ++ "}")
  , ("url", "${citationURL " ++ c ++ "}")
  ]

citationSets :: String -- ^ @'Citation'@
  -> [(String, String)]
citationSets c = linkSets c ++
  [ ("year", "${citationYear " ++ c ++ "}")
  ]

insertVolumeCitation :: TH.Name -- ^ @'AuditIdentity'@
  -> TH.Name -- ^ @'Volume'@
  -> TH.Name -- ^ @'Citation'@
  -> TH.ExpQ -- ^ ()
insertVolumeCitation ident v c = auditInsert ident "volume_citation"
  (volumeKeys (nameRef v) ++ citationSets (nameRef c))
  Nothing

updateVolumeCitation :: TH.Name -- ^ @'AuditIdentity'@
  -> TH.Name -- ^ @'Volume'@
  -> TH.Name -- ^ @'Citation'@
  -> TH.ExpQ -- ^ ()
updateVolumeCitation ident v c = auditUpdate ident "volume_citation"
  (citationSets (nameRef c))
  (whereEq $ volumeKeys (nameRef v))
  Nothing

deleteVolumeCitation :: TH.Name -- ^ @'AuditIdentity'@
  -> TH.Name -- ^ @'Volume'@
  -> TH.ExpQ -- ^ ()
deleteVolumeCitation ident v = auditDelete ident "volume_citation"
  (whereEq $ volumeKeys (nameRef v))
  Nothing

insertVolumeLink :: TH.Name -- ^ @'AuditIdentity'@
  -> TH.Name -- ^ @'Volume'@
  -> TH.Name -- ^ @'Citation'@
  -> TH.ExpQ -- ^ ()
insertVolumeLink ident v c = auditInsert ident "volume_link"
  (volumeKeys (nameRef v) ++ linkSets (nameRef c))
  Nothing

deleteVolumeLink :: TH.Name -- ^ @'AuditIdentity'@
  -> TH.Name -- ^ @'Volume'@
  -> TH.ExpQ -- ^ ()
deleteVolumeLink ident v = auditDelete ident "volume_link"
  (whereEq $ volumeKeys (nameRef v))
  Nothing
