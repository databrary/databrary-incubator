module Databrary.Model.AssetRevision.Types
  ( AssetRevision(..)
  ) where

import Databrary.Model.Asset.Types

data AssetRevision = AssetRevision
  { revisionAsset :: !Asset
  , revisionOrig :: !Asset
  }
