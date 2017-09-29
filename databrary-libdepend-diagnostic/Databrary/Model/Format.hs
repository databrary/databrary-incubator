{-# LANGUAGE TemplateHaskell, OverloadedStrings, RecordWildCards, ViewPatterns #-}
module Databrary.Model.Format
  ( module Databrary.Model.Format.Types
  , mimeTypeTop
  , mimeTypeSub
  , mimeTypeTopCompare
  , unknownFormat
  , allFormats
  , getFormat
  , getFormat'
  , getFormatByExtension
  , addFormatExtension
  , getFormatByFilename
  , dropFormatExtension
  , videoFormat
  , imageFormat
  , audioFormat
  , formatIsImage
  , formatTranscodable
  , formatSample
  , formatJSON
  ) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.Char (toLower)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Map.Strict as Map
import Data.Maybe (listToMaybe)
import Data.Monoid ((<>))
import System.Posix.FilePath (RawFilePath, splitExtension, takeExtension, addExtension)

import qualified Databrary.JSON as JSON
import Databrary.Model.Id
import Databrary.Model.Format.Types
import Databrary.Model.Format.Boot

mimeTypes :: BS.ByteString -> (BS.ByteString, BS.ByteString)
mimeTypes s = maybe (s, "") (\i -> (BS.take i s, BS.drop (succ i) s)) $ BSC.elemIndex '/' s

mimeTypeTop, mimeTypeSub :: BS.ByteString -> BS.ByteString
mimeTypeTop = fst . mimeTypes
mimeTypeSub = snd . mimeTypes

mimeTypeTopCompare :: BS.ByteString -> BS.ByteString -> Ordering
mimeTypeTopCompare a b = mttc (BSC.unpack a) (BSC.unpack b) where
  mttc []      []      = EQ
  mttc ('/':_) []      = EQ
  mttc []      ('/':_) = EQ
  mttc ('/':_) ('/':_) = EQ
  mttc ('/':_) _       = LT
  mttc []      _       = LT
  mttc _       ('/':_) = GT
  mttc _       []      = GT
  mttc (ac:as) (bc:bs) = compare ac bc <> mttc as bs

unknownFormat :: Format
unknownFormat = Format
  { formatId = error "unknownFormat"
  , formatMimeType = "application/octet-stream"
  , formatExtension = []
  , formatName = "Unknown"
  }

allFormats :: [Format]
allFormats = $(loadFormats)

formatsById :: IntMap.IntMap Format
formatsById = IntMap.fromAscList $ map (\a -> (fromIntegral $ unId $ formatId a, a)) allFormats

getFormat :: Id Format -> Maybe Format
getFormat (Id i) = IntMap.lookup (fromIntegral i) formatsById

getFormat' :: Id Format -> Format
getFormat' (Id i) = formatsById IntMap.! fromIntegral i

formatsByExtension :: Map.Map BS.ByteString Format
formatsByExtension = Map.fromList [ (e, a) | a <- allFormats, e <- formatExtension a ]

getFormatByExtension :: BS.ByteString -> Maybe Format
getFormatByExtension e = Map.lookup (BSC.map toLower e) formatsByExtension

addFormatExtension :: RawFilePath -> Format -> RawFilePath
addFormatExtension p (formatExtension -> (e:_)) = addExtension p e
addFormatExtension p _ = p

getFormatByFilename :: RawFilePath -> Maybe Format
getFormatByFilename n = do
  ('.',e) <- BSC.uncons $ takeExtension n
  getFormatByExtension e

dropFormatExtension :: Format -> RawFilePath -> RawFilePath
dropFormatExtension fmt n
  | (f,BSC.uncons -> Just ('.',e)) <- splitExtension n
  , BSC.map toLower e `elem` formatExtension fmt = f
  | otherwise = n

videoFormat :: Format
videoFormat = getFormat' (Id (-800))

imageFormat :: Format
imageFormat = getFormat' (Id (-700))

audioFormat :: Format
audioFormat = getFormat' (Id (-600))

formatIsVideo :: Format -> Bool
formatIsVideo Format{ formatMimeType = t } = "video/" `BS.isPrefixOf` t

formatIsImage :: Format -> Bool
formatIsImage Format{ formatMimeType = t } = "image/" `BS.isPrefixOf` t

formatIsAudio :: Format -> Bool
formatIsAudio Format{ formatMimeType = t } = "audio/" `BS.isPrefixOf` t

formatTranscodable :: Format -> Maybe Format
formatTranscodable f
  | formatIsVideo f = Just videoFormat
  | formatIsAudio f = Just audioFormat
  | otherwise = Nothing

formatSample :: Format -> Maybe Format
formatSample f
  | f == videoFormat = Just imageFormat
  | otherwise = Nothing

formatJSON :: JSON.ToObject o => Format -> JSON.Record (Id Format) o
formatJSON f = JSON.Record (formatId f) $
     "mimetype" JSON..= formatMimeType f
  <> "extension" JSON..=? listToMaybe (formatExtension f)
  <> "name" JSON..= formatName f
  <> "transcodable" JSON..=? (formatId <$> formatTranscodable f)
  -- TODO: description
