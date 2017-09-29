{-# LANGUAGE OverloadedStrings, TypeOperators, QuasiQuotes #-}
module Databrary.Controller.Paths
  ( pathId
  , PartyTarget(..)
  , pathPartyTarget
  , AuthorizeTarget(..)
  , pathAuthorizeTarget
  , VolumeAccessTarget(..)
  , pathVolumeAccessTarget
  , pathSegment
  , pathSlotId
  , TagId(..)
  , pathTagId
  ) where

import qualified Data.Invertible as I
import Data.String (fromString)
import qualified Web.Route.Invertible as R

import Databrary.Model.Kind
import Databrary.Model.Id.Types
import Databrary.Model.Party.Types
import Databrary.Model.Container.Types
import Databrary.Model.Segment
import Databrary.Model.Slot.Types
import Databrary.Model.Tag.Types
import Databrary.HTTP.Path
import Databrary.HTTP.Path.Parser

idIso :: IdType a I.<-> Id a
idIso = [I.biCase|a <-> Id a|]

pathIdWith :: forall a . (Kinded a) => PathParser (IdType a) -> PathParser (Id a)
pathIdWith p = fromString (kindOf (undefined :: a)) >/> idIso >$< p

pathId :: forall a . (PathParameter (IdType a), Kinded a) => PathParser (Id a)
pathId = pathIdWith R.parameter

data PartyTarget
  = TargetProfile
  | TargetParty (Id Party)

pathPartyTarget :: PathParser PartyTarget
pathPartyTarget = [I.biCase|
    Left () <-> TargetProfile
    Right i <-> TargetParty i
  |] >$< ("profile" |/| pathId)

data AuthorizeTarget = AuthorizeTarget
  { authorizeApply :: Bool
  , authorizeTarget :: Id Party
  }

pathAuthorizeTarget :: PathParser AuthorizeTarget
pathAuthorizeTarget = [I.biCase|(a, t) <-> AuthorizeTarget a t|] >$<
  (I.isRight >$< ("authorize" |/| "apply")
   </> idIso >$< R.parameter)

newtype VolumeAccessTarget = VolumeAccessTarget
  { volumeAccessTarget :: Id Party
  }

pathVolumeAccessTarget :: PathParser VolumeAccessTarget
pathVolumeAccessTarget = "access" >/> [I.biCase|i <-> VolumeAccessTarget (Id i)|] >$< R.parameter

slotIdIso :: (Id Container, Segment) I.<-> SlotId
slotIdIso = [I.biCase|(c, s) <-> SlotId c s|]

pathSegment :: PathParser Segment
pathSegment = fullSegment =/= R.parameter

pathSlot :: PathParser SlotId
pathSlot = slotIdIso >$< (idIso >$< R.parameter </> pathSegment)

pathSlotId :: PathParser (Id Slot)
pathSlotId = pathIdWith pathSlot

data TagId = TagId
  { tagIdKeyword :: Bool
  , tagIdName :: TagName
  }

pathTagId :: PathParser TagId
pathTagId = [I.biCase|(b, t) <-> TagId b t|] >$<
  (I.isRight >$< ("tag" |/| "keyword") </> R.parameter)

