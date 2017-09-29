module Databrary.Web
  ( WebFilePath
  , webFileRel
  , webFileAbs
  , webFileRelRaw
  , webFileAbsRaw
  , webDir
  , webDirRaw
  , splitWebExtensions
  , splitWebExtension
  , replaceWebExtension
  ) where

import Control.Arrow (first)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.Function (on)
import Data.Hashable (Hashable(..))
import Data.String (IsString(..))
import qualified System.FilePath as FP
import System.IO.Unsafe (unsafeDupablePerformIO)
import qualified System.Posix.FilePath as RFP

import Paths_databrary (getDataFileName)
import Databrary.Files

data WebFilePath = WebFilePath
  { webFileRel, webFileAbs :: FilePath
  , webFileRelRaw, webFileAbsRaw :: RawFilePath
  }

instance Eq WebFilePath where
  (==) = (==) `on` webFileRelRaw
  (/=) = (/=) `on` webFileRelRaw
instance Ord WebFilePath where
  compare = compare `on` webFileRelRaw
instance Hashable WebFilePath where
  hashWithSalt n = hashWithSalt n . webFileRelRaw
  hash = hash . webFileRelRaw

instance Show WebFilePath where
  showsPrec p = showsPrec p . ("web" FP.</>) . webFileRel

webDir :: FilePath
webDir = unsafeDupablePerformIO $ getDataFileName "web"

webDirRaw :: RawFilePath
webDirRaw = toRawFilePath webDir

makeWebFilePath :: FilePath -> RawFilePath -> WebFilePath
makeWebFilePath f r = WebFilePath f (webDir FP.</> f) r (webDirRaw RFP.</> r)

webFilePath :: IsFilePath f => f -> WebFilePath
webFilePath f = makeWebFilePath (toFilePath f) (toRawFilePath f)

instance IsString WebFilePath where
  fromString = webFilePath

instance IsFilePath WebFilePath where
  toFilePath = webFileAbs
  toRawFilePath = webFileAbsRaw
  fromRawFilePath = webFilePath

  WebFilePath f fa r ra </> WebFilePath f' _ r' _ = WebFilePath (f FP.</> f') (fa FP.</> f') (r RFP.</> r') (ra RFP.</> r')
  WebFilePath f fa r ra <.> WebFilePath f' _ r' _ = WebFilePath (f FP.<.> f') (fa FP.<.> f') (r RFP.<.> r') (ra RFP.<.> r')

splitWebExtensions :: WebFilePath -> (WebFilePath, BS.ByteString)
splitWebExtensions f =
  first (makeWebFilePath (FP.dropExtensions $ webFileRel f)) $ RFP.splitExtensions $ webFileRelRaw f

splitWebExtension :: WebFilePath -> (WebFilePath, BS.ByteString)
splitWebExtension f =
  first (makeWebFilePath (FP.dropExtension $ webFileRel f)) $ RFP.splitExtension $ webFileRelRaw f

replaceWebExtension :: String -> WebFilePath -> WebFilePath
replaceWebExtension e (WebFilePath f fa r ra) = WebFilePath (FP.replaceExtension f e) (FP.replaceExtension fa e) (RFP.replaceExtension r re) (RFP.replaceExtension ra re) where re = BSC.pack e
