{-# LANGUAGE OverloadedStrings, TemplateHaskell, TypeFamilies #-}
module Databrary.Model.Volume.Types
  ( VolumeRow(..)
  , Volume(..)
  , VolumeOwner
  , blankVolume
  ) where

import qualified Data.ByteString as BS
import qualified Data.Text as T
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Language.Haskell.TH.Lift (deriveLiftMany)

import Databrary.Has (makeHasRec)
import Databrary.Model.Time
import Databrary.Model.Kind
import Databrary.Model.Permission.Types
import Databrary.Model.Id.Types
import Databrary.Model.Party.Types

type instance IdType Volume = Int32

data VolumeRow = VolumeRow
  { volumeId :: Id Volume
  , volumeName :: T.Text
  , volumeBody :: Maybe T.Text
  , volumeAlias :: Maybe T.Text
  , volumeDOI :: Maybe BS.ByteString
  }

type VolumeOwner = (Id Party, T.Text)

data Volume = Volume
  { volumeRow :: !VolumeRow
  , volumeCreation :: Timestamp
  , volumeOwners :: [VolumeOwner]
  , volumePermission :: Permission
  }

instance Kinded Volume where
  kindOf _ = "volume"

makeHasRec ''VolumeRow ['volumeId]
makeHasRec ''Volume ['volumeRow, 'volumePermission]
deriveLiftMany [''VolumeRow, ''Volume]

blankVolume :: Volume
blankVolume = Volume
  { volumeRow = VolumeRow
    { volumeId = error "blankVolume"
    , volumeName = ""
    , volumeAlias = Nothing
    , volumeBody = Nothing
    , volumeDOI = Nothing
    }
  , volumeCreation = posixSecondsToUTCTime 1357900000
  , volumeOwners = []
  , volumePermission = PermissionNONE
  }
