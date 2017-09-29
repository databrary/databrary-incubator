{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Databrary.Model.RecordSlot.SQL
  ( selectRecordContainerSlotRecord
  , selectContainerSlotRecord
  , selectRecordSlotRecord
  , selectVolumeSlotRecord
  , selectVolumeSlotIdRecord
  , selectVolumeSlotMaybeRecord
  , selectVolumeSlotMaybeRecordId
  , selectSlotRecord
  , insertSlotRecord
  , updateSlotRecord
  , deleteSlotRecord
  ) where

import qualified Language.Haskell.TH as TH

import Databrary.Has (view)
import Databrary.Model.Id.Types
import Databrary.Model.Segment
import Databrary.Model.Volume.Types
import Databrary.Model.Record.Types
import Databrary.Model.Record.SQL
import Databrary.Model.Container.Types
import Databrary.Model.Container.SQL
import Databrary.Model.Slot.Types
import Databrary.Model.SQL.Select
import Databrary.Model.Audit.SQL
import Databrary.Model.Volume.SQL
import Databrary.Model.RecordSlot.Types

makeSlotRecord :: Segment -> Record -> Container -> RecordSlot
makeSlotRecord seg r c = RecordSlot r (Slot c seg)

selectRecordContainerSlotRecord :: Selector -- ^ @'Record' -> 'Container' -> 'RecordSlot'@
selectRecordContainerSlotRecord = selectMap (TH.VarE 'makeSlotRecord `TH.AppE`) (selectColumn "slot_record" "segment")

makeContainerSlotRecord :: (Record -> Container -> RecordSlot) -> (Volume -> Record) -> Container -> RecordSlot
makeContainerSlotRecord f rf c = f (rf (view c)) c

selectContainerSlotRecord :: Selector -- ^ @'Container' -> 'RecordSlot'@
selectContainerSlotRecord = selectJoin 'makeContainerSlotRecord
  [ selectRecordContainerSlotRecord
  , joinOn "slot_record.record = record.id"
    selectVolumeRecord -- XXX volumes match?
  ]

makeRecordSlotRecord :: (Record -> Container -> RecordSlot) -> (Volume -> Container) -> Record -> RecordSlot
makeRecordSlotRecord f cf r = f r (cf (view r))

selectRecordSlotRecord :: Selector -- ^ @'Record' -> 'RecordSlot'@
selectRecordSlotRecord = selectJoin 'makeRecordSlotRecord
  [ selectRecordContainerSlotRecord
  , joinOn "slot_record.container = container.id"
    selectVolumeContainer -- XXX volumes match?
  ]

makeVolumeSlotRecord :: (Record -> Container -> RecordSlot) -> (Volume -> Record) -> (Volume -> Container) -> Volume -> RecordSlot
makeVolumeSlotRecord f rf cf v = f (rf v) (cf v)

selectVolumeSlotRecord :: Selector -- ^ @'Volume' -> 'RecordSlot'@
selectVolumeSlotRecord = selectJoin 'makeVolumeSlotRecord
  [ selectRecordContainerSlotRecord
  , joinOn "slot_record.record = record.id"
    selectVolumeRecord
  , joinOn "slot_record.container = container.id AND record.volume = container.volume"
    selectVolumeContainer
  ]

makeVolumeSlotIdRecord :: SlotId -> (Volume -> Record) -> Volume -> (Record, SlotId)
makeVolumeSlotIdRecord s rf v = (rf v, s)

selectVolumeSlotIdRecord :: Selector -- ^ @'Volume' -> ('Record', 'SlotId')@
selectVolumeSlotIdRecord = selectJoin 'makeVolumeSlotIdRecord
  [ selectColumns 'SlotId "slot_record" ["container", "segment"]
  , joinOn "slot_record.record = record.id"
    selectVolumeRecord --- XXX volumes match?
  ]

makeVolumeSlotMaybeRecord :: (Volume -> Container) -> Maybe (Container -> RecordSlot) -> Volume -> (Container, Maybe RecordSlot)
makeVolumeSlotMaybeRecord cf Nothing v = (cf v, Nothing)
makeVolumeSlotMaybeRecord cf (Just rf) v = (c, Just (rf c)) where c = cf v

selectVolumeSlotMaybeRecord :: Selector -- ^ @'Volume' -> ('Container, Maybe 'RecordSlot)@
selectVolumeSlotMaybeRecord = selectJoin 'makeVolumeSlotMaybeRecord
  [ selectVolumeContainer
  , maybeJoinOn "container.id = slot_record.container AND container.volume = record.volume"
    selectContainerSlotRecord
  ]

segmentRecordIdTuple :: Segment -> Id Record -> (Segment, Id Record)
segmentRecordIdTuple = (,)

makeVolumeContainerTuple :: (Volume -> Container) -> a -> Volume -> (Container, a)
makeVolumeContainerTuple cf a v = (cf v, a)

selectVolumeSlotMaybeRecordId :: Selector -- ^ @'Volume' -> ('Container', Maybe ('Segment', 'Id' 'Record'))@
selectVolumeSlotMaybeRecordId = selectJoin 'makeVolumeContainerTuple
  [ selectVolumeContainer
  , maybeJoinOn "container.id = slot_record.container"
    $ selectColumns 'segmentRecordIdTuple "slot_record" ["segment", "record"]
  ]

selectSlotRecord :: TH.Name -- ^ @'Identity'@
  -> Selector -- ^ @'RecordSlot'@
selectSlotRecord ident = selectJoin '($)
  [ selectVolumeSlotRecord
  , joinOn "record.volume = volume.id"
    $ selectVolume ident
  ]

slotRecordVals :: String -- ^ @'RecordSlot'@
  -> [(String, String)]
slotRecordVals o =
  [ ("record", "${recordId $ recordRow $ slotRecord " ++ o ++ "}")
  , ("container", "${containerId $ containerRow $ slotContainer $ recordSlot " ++ o ++ "}")
  , ("segment", "${slotSegment $ recordSlot " ++ o ++ "}")
  ]

insertSlotRecord :: TH.Name -- ^ @'AuditIdentity'@
  -> TH.Name -- ^ @'RecordSlot'@
  -> TH.ExpQ
insertSlotRecord ident o = auditInsert ident "slot_record"
  (slotRecordVals os)
  Nothing
  where os = nameRef o

updateSlotRecord :: TH.Name -- ^ @'AuditIdentity'@
  -> TH.Name -- ^ @'RecordSlot'@
  -> TH.Name -- ^ @'Segment'@
  -> TH.ExpQ
updateSlotRecord ident o ds = auditUpdate ident "slot_record"
  [ ("segment", "${" ++ nameRef ds ++ "}") ]
  (whereEq $ slotRecordVals $ nameRef o)
  Nothing

deleteSlotRecord :: TH.Name -- ^ @'AuditIdentity'@
  -> TH.Name -- ^ @'RecordSlot'@
  -> TH.ExpQ
deleteSlotRecord ident o = auditDelete ident "slot_record"
  ("record = ${recordId $ recordRow $ slotRecord " ++ os ++ "} AND container = ${containerId $ containerRow $ slotContainer $ recordSlot " ++ os ++ "} AND segment <@ ${slotSegment $ recordSlot " ++ os ++ "}")
  Nothing
  where os = nameRef o
