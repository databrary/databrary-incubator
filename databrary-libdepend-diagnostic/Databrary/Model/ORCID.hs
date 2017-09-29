{-# LANGUAGE TemplateHaskell, DataKinds #-}
module Databrary.Model.ORCID
  ( ORCID
  , blankORCID
  , orcidURL
  ) where

import Control.Monad (guard)
import qualified Data.ByteString.Char8 as BSC
import Data.Char (isDigit, digitToInt, intToDigit)
import Data.List (foldl')
import Database.PostgreSQL.Typed.Types (PGParameter(..), PGColumn(..))
import Instances.TH.Lift ()
import Language.Haskell.TH.Lift (deriveLift)
import qualified Network.URI as URI
import qualified Text.ParserCombinators.ReadP as RP
import qualified Text.ParserCombinators.ReadPrec as RP (lift)
import Text.Read (Read(readPrec))

newtype ORCID = ORCID { orcid :: BSC.ByteString }

instance PGParameter "bpchar" ORCID where
  pgEncode t = pgEncode t . orcid
  pgEncodeValue e t = pgEncodeValue e t . orcid
  pgLiteral t = pgLiteral t . orcid
instance PGColumn "bpchar" ORCID where
  pgDecode t = ORCID . pgDecode t
  pgDecodeValue e t = ORCID . pgDecodeValue e t

deriveLift ''ORCID

checksumDigit :: Int -> Char
checksumDigit 10 = 'X'
checksumDigit i = intToDigit i

instance Show ORCID where
  show (ORCID s) = group $ BSC.unpack s where
    group (a:b:c:d:r@(_:_)) = a:b:c:d:'-':group r
    group r = r

instance Read ORCID where
  readPrec = RP.lift $ do
    RP.skipSpaces
    RP.optional $ RP.string "http://"
    RP.optional $ RP.string "orcid.org/"
    b <- RP.count 15 $ do
      RP.optional $ RP.char '-'
      RP.satisfy isDigit
    RP.optional $ RP.char '-'
    c <- RP.satisfy (\c -> 'X' == c || isDigit c)
    guard $ checksumDigit (10 - (9 + foldl' (\s -> (*) 2 . (+) s . digitToInt) 0 b) `mod` 11) == c
    return $ ORCID $ BSC.snoc (BSC.pack b) c

blankORCID :: ORCID
blankORCID = ORCID BSC.empty -- "0000000000000001"

orcidURL :: ORCID -> URI.URI
orcidURL o = URI.nullURI
  { URI.uriScheme = "http:"
  , URI.uriAuthority = Just URI.URIAuth
    { URI.uriUserInfo = ""
    , URI.uriRegName = "orcid.org"
    , URI.uriPort = ""
    }
  , URI.uriPath = '/' : show o
  }
