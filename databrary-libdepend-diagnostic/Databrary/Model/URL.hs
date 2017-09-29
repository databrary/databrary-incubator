{-# LANGUAGE TemplateHaskell, DataKinds #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Databrary.Model.URL
  ( URI
  , validHDL
  , hdlURL
  , parseURL
  ) where

import Control.Monad ((<=<), guard)
import Data.Aeson (ToJSON(..))
import Data.Char (isDigit)
import Data.Maybe (fromMaybe, isNothing)
import Database.PostgreSQL.Typed.Types (PGParameter(..), PGColumn(..))
import Language.Haskell.TH.Lift (deriveLiftMany)
import Network.URI
import qualified Text.Blaze as H

import qualified Databrary.Store.Config as C

toPG :: URI -> String
toPG u = uriToString id u ""

fromPG :: String -> URI
fromPG u = fromMaybe (error $ "pgDecode URI: " ++ u) $ parseURI u

instance PGParameter "text" URI where
  pgEncode t = pgEncode t . toPG
  pgEncodeValue e t = pgEncodeValue e t . toPG
  pgLiteral t = pgLiteral t . toPG
instance PGColumn "text" URI where
  pgDecode t = fromPG . pgDecode t
  pgDecodeValue e t = fromPG . pgDecodeValue e t

instance ToJSON URI where
  toJSON = toJSON . show

instance C.Configurable URI where
  config = parseAbsoluteURI <=< C.config

instance H.ToValue URI where
  toValue = H.stringValue . show . urlLink
  preEscapedToValue = H.preEscapedStringValue . show . urlLink

validHDL :: String -> Bool
validHDL = v0 (0 :: Int) where
  v0 n (c:s) | isDigit c = v1 n s
  v0 _ _ = False
  v1 n ('/':_) = n > 0
  v1 n ('.':s) = v0 (succ n) s
  v1 n s = v0 n s

hdlURL :: String -> URI
hdlURL doi = URI "hdl:" Nothing doi "" ""

parseURL :: String -> Maybe URI
parseURL d@('1':'0':'.':c:_) | isDigit c = parseURL $ "doi:" ++ d
parseURL s = do
  u <- parseURI s
  if uriScheme u `elem` ["doi:","hdl:"] && isNothing (uriAuthority u) ||
     uriScheme u == "http:" && (uriAuthority u == Just (URIAuth "" "dx.doi.org" "") || uriAuthority u == Just (URIAuth "" "doi.org" ""))
    then do
      let p = dropWhile ('/' ==) $ uriPath u
      guard $ validHDL p
      return u
        { uriScheme = "hdl:"
        , uriAuthority = Nothing
        , uriPath = p
        }
    else do
      guard $ uriScheme u `elem` ["http:","https:"]
      return u

httpAuth :: String -> URI -> URI
httpAuth a u = u{ uriScheme = "http:", uriAuthority = Just (URIAuth "" a ""), uriPath = '/':uriPath u }

urlLink :: URI -> URI
urlLink u@URI{ uriScheme = "hdl:" } = httpAuth "hdl.handle.net" u
urlLink u@URI{ uriScheme = "doi:" } = httpAuth "doi.org" u
urlLink u = u

deriveLiftMany [''URIAuth, ''URI]
