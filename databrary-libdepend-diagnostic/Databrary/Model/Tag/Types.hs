{-# LANGUAGE OverloadedStrings, TemplateHaskell, TypeFamilies, DataKinds, GeneralizedNewtypeDeriving, DeriveDataTypeable #-}
module Databrary.Model.Tag.Types
  ( TagName(..)
  , validateTag
  , Tag(..)
  , TagUse(..)
  , TagUseRow(..)
  , TagCoverage(..)
  , TagWeight(..)
  ) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.Char (toLower)
import qualified Data.Text.Encoding as TE
import Data.Typeable (Typeable)
import Database.PostgreSQL.Typed.Types (PGParameter(..), PGColumn(..))
import qualified Text.Regex.Posix as Regex
import qualified Web.Route.Invertible as R

import Databrary.Ops
import Databrary.Has (makeHasRec)
import qualified Databrary.JSON as JSON
import Databrary.Model.Kind
import Databrary.Model.Id.Types
import Databrary.Model.Party.Types
import Databrary.Model.Container.Types
import Databrary.Model.Segment
import Databrary.Model.Slot.Types

newtype TagName = TagName { tagNameBS :: BS.ByteString } deriving (JSON.ToJSON, JSON.FromJSON, Typeable)

validTag :: Regex.Regex
validTag = Regex.makeRegex
  ("^[a-z][-a-z ]+[a-z]$" :: BS.ByteString)

validateTag :: BS.ByteString -> Maybe TagName
validateTag t = Regex.matchTest validTag tt ?> TagName tt where
  tt = BSC.map toLower $ BSC.unwords $ BSC.words t

instance R.Parameter R.PathString TagName where
  parseParameter = validateTag . TE.encodeUtf8
  renderParameter = TE.decodeLatin1 . tagNameBS

instance PGParameter "character varying" TagName where
  pgEncode t (TagName n) = pgEncode t n
  pgEncodeValue e t (TagName n) = pgEncodeValue e t n
  pgLiteral t (TagName n) = pgLiteral t n
instance PGColumn "character varying" TagName where
  pgDecode t = TagName . pgDecode t
  pgDecodeValue e t = TagName . pgDecodeValue e t

type instance IdType Tag = Int32

data Tag = Tag
  { tagId :: Id Tag
  , tagName :: TagName
  }

makeHasRec ''Tag ['tagId, 'tagName]

instance Kinded Tag where
  kindOf _ = "tag"

data TagUse = TagUse
  { useTag :: Tag
  , tagKeyword :: Bool
  , tagWho :: Account
  , tagSlot :: Slot
  }

makeHasRec ''TagUse ['useTag, 'tagWho, 'tagSlot]

data TagUseRow = TagUseRow
  { useTagRow :: Tag
  , tagRowKeyword :: Bool
  , tagRowWhoId :: Id Party
  , tagRowSlotId :: SlotId
  }

data TagWeight = TagWeight
  { tagWeightTag :: Tag
  , tagWeightWeight :: Int32
  }

makeHasRec ''TagWeight ['tagWeightTag]

data TagCoverage = TagCoverage
  { tagCoverageWeight :: !TagWeight
  , tagCoverageContainer :: Container
  , tagCoverageSegments
  , tagCoverageKeywords
  , tagCoverageVotes :: [Segment]
  }

makeHasRec ''TagCoverage ['tagCoverageWeight, 'tagCoverageContainer]
