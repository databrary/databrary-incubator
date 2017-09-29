{-# LANGUAGE TemplateHaskell #-}
module Databrary.Model.Comment.SQL
  ( selectContainerComment
  , selectComment
  ) where

import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Language.Haskell.TH as TH

import Databrary.Model.SQL.Select
import Databrary.Model.Time
import Databrary.Model.Id.Types
import Databrary.Model.Party.Types
import Databrary.Model.Party.SQL
import Databrary.Model.Comment.Types
import Databrary.Model.Container.Types
import Databrary.Model.Container.SQL
import Databrary.Model.Segment
import Databrary.Model.Slot.Types

selectAccountContainerComment :: Selector -- ^ @'Account' -> 'Container' -> 'Comment'@
selectAccountContainerComment = 
  fromMap 
    ("comment_thread AS " ++) 
    (selectColumns 'makeComment "comment" ["id", "segment", "time", "text", "thread"])

selectContainerComment :: TH.Name -- ^ @'Identity'@
  -> Selector -- ^ @'Container' -> 'Comment'@
selectContainerComment ident = selectJoin '($)
  [ selectAccountContainerComment
  , joinOn "comment.who = account.id"
    $ selectAccount ident
  ]

selectComment :: TH.Name -- ^ @'Identity'@
  -> Selector -- ^ @'Comment'@
selectComment ident = selectJoin '($)
  [ selectContainerComment ident
  , joinOn "comment.container = container.id"
    $ selectContainer ident
  ]

_commentSets :: String -- ^ @'Comment'@
  -> [(String, String)]
_commentSets o =
  [ ("who", "${partyId $ accountParty $ commentWho " ++ o ++ "}")
  , ("container", "${containerId $ slotContainer $ commentSlot " ++ o ++ "}")
  , ("segment", "${slotSegment $ commentSlot " ++ o ++ "}")
  , ("text", "${commentText " ++ o ++ "}")
  , ("parent", "${listToMaybe $ commentParents " ++ o ++ "}")
  ]
