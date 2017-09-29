{-# LANGUAGE TemplateHaskell, TypeFamilies, OverloadedStrings #-}
module Databrary.Model.Token.Types
  ( Token(..)
  , AccountToken(..)
  , LoginToken(..)
  , Session(..)
  , Upload(..)
  ) where

import qualified Data.ByteString as BS
import Data.Int (Int64)

import Databrary.Has (makeHasRec)
import Databrary.Model.Kind
import Databrary.Model.Time
import Databrary.Model.Id.Types
import Databrary.Model.Party.Types

type instance IdType Token = BS.ByteString

data Token = Token
  { tokenId :: Id Token
  , tokenExpires :: Timestamp
  }

makeHasRec ''Token ['tokenId]

data AccountToken = AccountToken
  { accountToken :: !Token
  , tokenAccount :: SiteAuth
  }

makeHasRec ''AccountToken ['accountToken, 'tokenAccount]

data LoginToken = LoginToken
  { loginAccountToken :: !AccountToken
  , loginPasswordToken :: Bool
  }

-- these are signed version of Id Token
type instance IdType LoginToken = BS.ByteString

instance Kinded LoginToken where
  kindOf _ = "token"

makeHasRec ''LoginToken ['loginAccountToken]

data Session = Session
  { sessionAccountToken :: !AccountToken
  , sessionVerf :: !BS.ByteString
  , sessionSuperuser :: Bool
  }

makeHasRec ''Session ['sessionAccountToken]

data Upload = Upload
  { uploadAccountToken :: AccountToken
  , uploadFilename :: BS.ByteString
  , uploadSize :: Int64
  }

makeHasRec ''Upload ['uploadAccountToken]
