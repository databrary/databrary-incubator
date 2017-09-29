{-# LANGUAGE ViewPatterns #-}
module Databrary.Store.Asset
  ( maxAssetSize
  , assetFile
  , getAssetFile
  , storeAssetFile
  ) where

import Control.Monad ((<=<), unless)
import Control.Monad.IO.Class (liftIO)
import Crypto.Hash (Digest, SHA1)
import Data.ByteArray (convert)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Lazy as BSL
import Data.Word (Word64)
import System.Posix.FilePath (takeDirectory)
import System.Posix.Files.ByteString (fileSize, createLink)

import Databrary.Ops
import Databrary.Has (peek, peeks)
import Databrary.Files
import Databrary.Store.Types
import Databrary.Model.Asset.Types

maxAssetSize :: Word64
maxAssetSize = 128*1024*1024*1024

assetFile :: Asset -> Maybe RawFilePath
assetFile = fmap sf . BS.uncons <=< assetSHA1 . assetRow where
  sf (h,t) = bs (BSB.word8HexFixed h) </> bs (BSB.byteStringHex t)
  bs = BSL.toStrict . BSB.toLazyByteString

getAssetFile :: MonadStorage c m => Asset -> m (Maybe RawFilePath)
getAssetFile a = do
  s <- peek
  let
    mf Nothing p = return $ storageMaster s </> p
    mf (Just sf) p = do
      me <- fileExist m
      if me
        then return m
        else do
          fe <- fileExist f
          return $ if fe then f else m
      where
      m = storageMaster s </> p
      f = sf </> p
  mapM (liftIO . mf (storageFallback s)) $ assetFile a

storeAssetFile :: MonadStorage c m => Asset -> RawFilePath -> m Asset
storeAssetFile ba@Asset{ assetRow = bar } fp = peeks storageMaster >>= \sm -> liftIO $ do
  size <- (fromIntegral . fileSize              <$> getFileStatus fp) `fromMaybeM` assetSize bar
  sha1 <- ((convert :: Digest SHA1 -> BS.ByteString) <$> hashFile fp) `fromMaybeM` assetSHA1 bar
  let a = ba{ assetRow = bar
        { assetSize = Just size
        , assetSHA1 = Just sha1
        } }
      Just af = assetFile a
      as = sm </> af
  ase <- fileExist as
  if ase
    then do
      sf <- compareFiles fp as
      unless sf $ fail "storage hash collision"
    else do
      _ <- createDir (takeDirectory as) 0o750
      createLink fp as
  return a
