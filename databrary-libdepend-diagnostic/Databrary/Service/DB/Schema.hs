{-# LANGUAGE OverloadedStrings #-}
module Databrary.Service.DB.Schema
  ( updateDBSchema
  ) where

import Control.Arrow (first, second)
import Control.Exception.Lifted (tryJust)
import Control.Monad (guard)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.ByteString.Lazy as BSL
import Data.Int (Int32)
import Data.List (sort)
import Database.PostgreSQL.Typed.Protocol (PGError, pgErrorCode)
import Database.PostgreSQL.Typed.Query (rawPGSimpleQuery)
import Database.PostgreSQL.Typed.Dynamic (pgDecodeRep, pgLiteralRep)
import System.Directory (getDirectoryContents)
import System.FilePath ((</>), (<.>), splitExtension)
import System.IO (stderr, hPutStr, hPutChar, hFlush)

import Databrary.Service.DB

playEvolution :: Int32
playEvolution = 85

schemaError :: Monad m => String -> m a
schemaError = fail

confirm :: MonadIO m => String -> m ()
confirm s = liftIO $ do
  hPutStr stderr s
  hPutChar stderr ' '
  hFlush stderr
  a <- getLine
  case a of
    ('y':_) -> return ()
    ('Y':_) -> return ()
    _ -> schemaError "Schema update aborted."

sqlFile :: FilePath -> DBM ()
sqlFile f = do
  sql <- liftIO $ BSL.readFile f
  dbExecute_ sql

schemaList :: [FilePath] -> Maybe [String]
schemaList l = case sort [ n | (n, ".sql") <- map splitExtension l ] of
  ("0":r) -> Just r
  _ -> Nothing

diffs :: [String] -> [String] -> ([String], [String])
diffs x [] = (x, [])
diffs [] y = ([], y)
diffs xa@(x:xl) ya@(y:yl) = case compare x y of
  LT -> first (x :) $ diffs xl ya
  EQ -> diffs xl yl
  GT -> second (y :) $ diffs xa yl

checkDNE :: PGError -> Maybe ()
checkDNE = (guard . ("42P01" ==) . pgErrorCode)

updateDBSchema :: FilePath -> DBM ()
updateDBSchema dir = do
  sl <- maybe (schemaError $ "Base schema " ++ show base ++ " not found") return
    =<< schemaList <$> liftIO (getDirectoryContents dir)

  lr <- tryJust checkDNE $ dbQuery
    $ pgDecodeRep . head <$> rawPGSimpleQuery "SELECT name FROM schema ORDER BY name"
  dl <- case lr of
    Left _ -> do
      pr <- tryJust checkDNE $ dbQuery1'
        $ pgDecodeRep . head <$> rawPGSimpleQuery "SELECT max(id) FROM play_evolutions WHERE state = 'applied'"
      case pr of
        Left _ -> do
          confirm "No schema found. Initialize?"
          sqlFile base
        Right n
          | n == playEvolution ->
            confirm "Migrate from play to schema?"
            -- dbExecute_ "DROP TABLE play_evolutions"
          | otherwise ->
            schemaError ("Play evolutions found but not up to date (expecting " ++ show playEvolution ++ " got " ++ show n ++ ")")
      dbExecute_ $ "CREATE TABLE schema (name varchar(64) Primary Key, applied timestamptz NOT NULL Default now())"
      return []
    Right l -> return l

  case diffs sl dl of
    (l, []) -> mapM_ apply l
    (_, e) -> schemaError $ "Inconsistent schema, missing: " ++ unwords (map show e)

  return ()
  where
  file = (dir </>) . (<.> ".sql")
  base = file "0"
  transact "0" = False
  transact _ = True
  apply n = do
    confirm $ "Apply schema " ++ show n ++ "?"
    if transact n then dbTransaction (run n) else run n
  run n = do
    dbExecute_ $ BSL.fromChunks ["INSERT INTO schema (name) VALUES (", pgLiteralRep n, ")"]
    sqlFile (file n)
