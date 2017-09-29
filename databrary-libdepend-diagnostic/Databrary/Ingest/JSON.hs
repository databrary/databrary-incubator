{-# LANGUAGE OverloadedStrings #-}
module Databrary.Ingest.JSON
  ( ingestJSON
  ) where

import Control.Arrow (left)
import Control.Monad (join, when, unless, void, mfilter, forM_)
import Control.Monad.Except (ExceptT(..), runExceptT, mapExceptT, catchError, throwError)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Trans.Class (lift)
import qualified Data.Aeson.BetterErrors as JE
import qualified Data.Attoparsec.ByteString as P
import qualified Data.ByteString as BS
import Data.ByteString.Lazy.Internal (defaultChunkSize)
import Data.Function (on)
import qualified Data.JsonSchema.Draft4 as JS
import Data.List (find)
import Data.Maybe (isJust, fromMaybe, isNothing)
import Data.Monoid ((<>))
import Data.Time.Format (parseTimeM, defaultTimeLocale)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Database.PostgreSQL.Typed.Range as Range
import System.IO (withBinaryFile, IOMode(ReadMode))

import Paths_databrary
import Databrary.Ops
import Databrary.Has (Has, view, focusIO)
import qualified Databrary.JSON as J
import Databrary.Files
import Databrary.Store.Stage
import Databrary.Store.Probe
import Databrary.Store.Transcode
import Databrary.Model.Time
import Databrary.Model.Kind
import Databrary.Model.Id.Types
import Databrary.Model.Volume
import Databrary.Model.Container
import Databrary.Model.Segment
import Databrary.Model.Slot.Types
import Databrary.Model.Release
import Databrary.Model.Record
import Databrary.Model.Category
import Databrary.Model.Metric
import Databrary.Model.Measure
import Databrary.Model.RecordSlot
import Databrary.Model.Asset
import Databrary.Model.AssetSlot
import Databrary.Model.AssetRevision
import Databrary.Model.Transcode
import Databrary.Model.Ingest
import Databrary.Action.Types

type IngestM a = JE.ParseT T.Text ActionM a

loadSchema :: ExceptT [T.Text] IO (J.Value -> [JS.Failure])
loadSchema = do
  schema <- lift $ getDataFileName "volume.json"
  r <- lift $ withBinaryFile schema ReadMode (\h ->
    P.parseWith (BS.hGetSome h defaultChunkSize) J.json' BS.empty)
  js <- ExceptT . return . left (return . T.pack) $ J.eitherJSON =<< P.eitherResult r
  ExceptT $ return $ left (map (T.pack . show)) $ JS.checkSchema (JS.SchemaCache js mempty) (JS.SchemaContext Nothing js)

throwPE :: T.Text -> IngestM a
throwPE = JE.throwCustomError

inObj :: forall a b . (Kinded a, Has (Id a) a, Show (IdType a)) => a -> IngestM b -> IngestM b
inObj o = JE.mapError (<> (" for " <> kindOf o <> T.pack (' ' : show (view o :: Id a))))

noKey :: T.Text -> IngestM ()
noKey k = void $ JE.keyMay k $ throwPE "unhandled value"

asKey :: IngestM IngestKey
asKey = JE.asText

asDate :: IngestM Date
asDate = JE.withString (maybe (Left "expecting %F") Right . parseTimeM True defaultTimeLocale "%F")

asRelease :: IngestM (Maybe Release)
asRelease = JE.perhaps JE.fromAesonParser

asCategory :: IngestM Category
asCategory =
  JE.withIntegral (err . getCategory . Id) `catchError` \_ -> do
    JE.withText (\n -> err $ find ((n ==) . categoryName) allCategories)
  where err = maybe (Left "category not found") Right

asSegment :: IngestM Segment
asSegment = JE.fromAesonParser

data StageFile = StageFile
  { stageFileRel :: !FilePath
  , stageFileAbs :: !FilePath
  }

asStageFile :: FilePath -> IngestM StageFile
asStageFile b = do
  r <- (b </>) <$> JE.asString
  a <- fromMaybeM (throwPE "stage file not found") =<< lift (focusIO (stageFile r))
  return $ StageFile r a

ingestJSON :: Volume -> J.Value -> Bool -> Bool -> ActionM (Either [T.Text] [Container])
ingestJSON vol jdata run overwrite = runExceptT $ do
  schema <- mapExceptT liftIO loadSchema
  let errs = schema jdata
  unless (null errs) $ throwError $ map (T.pack . show) errs
  if run
    then ExceptT $ left (JE.displayError id) <$> JE.parseValueM volume jdata
    else return []
  where
  check cur new
    | cur == new = return Nothing
    | not overwrite = throwPE $ "conflicting value: " <> T.pack (show new) <> " <> " <> T.pack (show cur)
    | otherwise = return $ Just new
  volume = do
    dir <- JE.keyOrDefault "directory" "" $ stageFileRel <$> asStageFile ""
    _ <- JE.keyMay "name" $ do
      name <- check (volumeName $ volumeRow vol) =<< JE.asText
      forM_ name $ \n -> lift $ changeVolume vol{ volumeRow = (volumeRow vol){ volumeName = n } }
    top <- lift (lookupVolumeTopContainer vol)
    JE.key "containers" $ JE.eachInArray (container top dir)
  container topc dir = do
    cid <- JE.keyMay "id" $ Id <$> JE.asIntegral
    key <- JE.key "key" $ asKey
    c' <- lift (lookupIngestContainer vol key)
    c <- maybe
      (do
        c <- maybe
          (do
            top <- JE.keyOrDefault "top" False JE.asBool
            name <- JE.keyMay "name" JE.asText
            date <- JE.keyMay "date" asDate
            let c = blankContainer vol
            lift $ addContainer c
              { containerRow = (containerRow c)
                { containerTop = top
                , containerName = name
                , containerDate = date
                }
              })
          (\i -> fromMaybeM (throwPE $ "container " <> T.pack (show i) <> "/" <> key <> " not found")
            =<< lift (lookupVolumeContainer vol i))
          cid
        inObj c $ lift $ addIngestContainer c key
        return c)
      (\c -> inObj c $ do
        unless (all (containerId (containerRow c) ==) cid) $ do
          throwPE "id mismatch"
        top <- fmap join . JE.keyMay "top" $ check (containerTop $ containerRow c) =<< JE.asBool
        name <- fmap join . JE.keyMay "name" $ check (containerName $ containerRow c) =<< JE.perhaps JE.asText
        date <- fmap join . JE.keyMay "date" $ check (containerDate $ containerRow c) =<< JE.perhaps asDate
        when (isJust top || isJust name || isJust date) $ lift $ changeContainer c
          { containerRow = (containerRow c)
            { containerTop = fromMaybe (containerTop $ containerRow c) top
            , containerName = fromMaybe (containerName $ containerRow c) name
            , containerDate = fromMaybe (containerDate $ containerRow c) date
            }
          }
        return c)
      c'
    let s = containerSlot c
    inObj c $ do
      _ <- JE.keyMay "release" $ do
        release <- maybe (return . fmap Just) (check . containerRelease) c' =<< asRelease
        forM_ release $ \r -> do
          o <- lift $ changeRelease s r
          unless o $ throwPE "update failed"
      _ <- JE.key "records" $ JE.eachInArray $ do
        r <- record
        inObj r $ do
          rs' <- lift $ lookupRecordSlotRecords r s
          segm <- (if null rs' then return . Just else check (map (slotSegment . recordSlot) rs')) =<< JE.keyOrDefault "positions" [fullSegment] (JE.eachInArray asSegment)
          forM_ segm $ \segs -> do
            let rs = RecordSlot r . Slot c
            unless (null rs') $ do
              o <- lift $ moveRecordSlot (rs fullSegment) emptySegment
              unless o $ throwPE "record clear failed"
            o <- lift $ mapM (moveRecordSlot (rs emptySegment)) segs
            unless (and o) $ throwPE "record link failed"
      _ <- JE.key "assets" $ JE.eachInArray $ do
        (a, probe) <- asset dir
        inObj a $ do
          as' <- lift $ mfilter (((/=) `on` containerId . containerRow) topc . slotContainer) . assetSlot <$> lookupAssetAssetSlot a
          seg <- JE.keyOrDefault "position" (maybe fullSegment slotSegment as') $
            JE.withTextM (\t -> if t == "auto"
              then maybe (Right . Segment . Range.point <$> probeAutoPosition c probe) (return . Right . slotSegment) $ mfilter (((==) `on` containerId . containerRow) c . slotContainer) as'
              else return $ Left "invalid asset position")
              `catchError` \_ -> asSegment
          let seg'
                | Just p <- Range.getPoint (segmentRange seg)
                , Just d <- assetDuration (assetRow a) = Segment $ Range.bounded p (p + d)
                | otherwise = seg
              ss = Slot c seg'
          u <- maybe (return True) (\s' -> isJust <$> on check slotId s' ss) as'
          when u $ do
            o <- lift $ changeAssetSlot $ AssetSlot a $ Just ss
            unless o $ throwPE "asset link failed"
      return c
  record = do
    rid <- JE.keyMay "id" $ Id <$> JE.asIntegral
    key <- JE.key "key" $ asKey
    r <- maybe
      (do
        r <- maybe
          (do
            category <- JE.key "category" asCategory
            lift $ addRecord $ blankRecord category vol)
          (\i -> fromMaybeM (throwPE $ "record " <> T.pack (show i) <> "/" <> key <> " not found")
            =<< lift (lookupVolumeRecord vol i))
          rid
        inObj r $ lift $ addIngestRecord r key
        return r)
      (\r -> inObj r $ do
        unless (all (recordId (recordRow r) ==) rid) $ do
          throwPE "id mismatch"
        _ <- JE.key "category" $ do
          category <- asCategory
          category' <- (category <$) <$> on check categoryName (recordCategory $ recordRow r) category
          forM_ category' $ \c -> lift $ changeRecord r
            { recordRow = (recordRow r)
              { recordCategory = c
              }
            }
        return r)
      =<< lift (lookupIngestRecord vol key)
    _ <- inObj r $ JE.forEachInObject $ \mn ->
      unless (mn `elem` ["id", "key", "category", "positions"]) $ do
        metric <- fromMaybeM (throwPE $ "metric " <> mn <> " not found") $ find (\m -> mn == metricName m && recordCategory (recordRow r) == metricCategory m) allMetrics
        datum <- maybe (return . Just) (check . measureDatum) (getMeasure metric (recordMeasures r)) . TE.encodeUtf8 =<< JE.asText
        forM_ datum $ lift . changeRecordMeasure . Measure r metric
    return r
  asset dir = do
    sa <- fromMaybeM
      (JE.key "file" $ do
        file <- asStageFile dir
        (,) . Just . (,) file
          <$> (either throwPE return
            =<< lift (probeFile (toRawFilePath $ stageFileRel file) (toRawFilePath $ stageFileAbs file)))
          <*> lift (lookupIngestAsset vol $ stageFileRel file))
      =<< (JE.keyMay "id" $ do
        maybe (throwPE "asset not found") (return . (,) Nothing . Just) =<< lift . lookupVolumeAsset vol . Id =<< JE.asIntegral)
    when (isNothing $ fst sa) $ noKey "file"
    orig <- JE.keyMay "replace" $
      let err = fmap $ maybe (Left "asset not found") Right in
      JE.withIntegralM (err . lookupVolumeAsset vol . Id) `catchError` \_ ->
        JE.withStringM (err . lookupIngestAsset vol)
    a <- case sa of
      (_, Just a) -> inObj a $ do
        unless (assetBacked a) $ throwPE "ingested asset incomplete"
        -- compareFiles file =<< getAssetFile -- assume correct
        release <- fmap join . JE.keyMay "release" $ check (assetRelease $ assetRow a) =<< asRelease
        name <- fmap join . JE.keyMay "name" $ check (assetName $ assetRow a) =<< JE.perhaps JE.asText
        a' <- if isJust release || isJust name
          then lift $ changeAsset a
            { assetRow = (assetRow a)
              { assetRelease = fromMaybe (assetRelease $ assetRow a) release
              , assetName = fromMaybe (assetName $ assetRow a) name
              }
            } Nothing
          else return a
        forM_ orig $ \o -> lift $ replaceSlotAsset o a'
        return a'
      (~(Just (file, probe)), Nothing) -> do
        release <- JE.key "release" asRelease
        name <- JE.keyMay "name" JE.asText
        let ba = blankAsset vol
        a <- lift $ addAsset ba
          { assetRow = (assetRow ba)
            { assetFormat = probeFormat probe
            , assetRelease = release
            , assetName = name
            }
          } (Just $ toRawFilePath $ stageFileAbs file)
        lift $ addIngestAsset a (stageFileRel file)
        forM_ orig $ \o -> lift $ replaceAsset o a -- FIXME
        return a
    inObj a $ case sa of
      (Just (_, probe@ProbeAV{}), ae) -> do
        clip <- JE.keyOrDefault "clip" fullSegment asSegment
        opts <- JE.keyOrDefault "options" defaultTranscodeOptions $ JE.eachInArray JE.asString
        t <- lift $ fromMaybeM
          (do
            t <- addTranscode a clip opts probe
            _ <- startTranscode t
            return t)
          =<< flatMapM (\_ -> findTranscode a clip opts) ae
        return (transcodeAsset t, Just probe)
      _ -> do
        noKey "clip"
        noKey "options"
        return (a, Nothing)
