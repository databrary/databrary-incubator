{-# LANGUAGE OverloadedStrings #-}
module Databrary.Model.SQL
  ( selectQuery
  , isUniqueViolation
  , isExclusionViolation
  , isForeignKeyViolation
  , tryUpdateOrInsert
  , updateOrInsert
  ) where

import Control.Monad (guard)
import Database.PostgreSQL.Typed.Protocol (PGError(..), pgErrorCode)
import Database.PostgreSQL.Typed.Query (PGQuery)
import qualified Language.Haskell.TH as TH

import Databrary.Service.DB
import Databrary.Model.SQL.Select

selectQuery :: Selector -> String -> TH.ExpQ
selectQuery = selectDistinctQuery Nothing

isUniqueViolation, isExclusionViolation, isForeignKeyViolation :: PGError -> Bool
isUniqueViolation = ("23505" ==) . pgErrorCode
isExclusionViolation e = pgErrorCode e `elem` ["23505","23P01"]
isForeignKeyViolation = ("23503" ==) . pgErrorCode

tryUpdateOrInsert :: (MonadDB c m, PGQuery q a) => (PGError -> Maybe e) -> q -> q -> m (Either e (Int, [a]))
tryUpdateOrInsert err upd ins = dbTransaction uoi where
  err' e
    | isUniqueViolation e = Just Nothing
    | otherwise = Just <$> err e
  uoi = do
    u <- dbTryJust err $ dbRunQuery upd
    case u of
      Right (0, _) -> do
        _ <- dbExecuteSimple "SAVEPOINT pre_insert"
        i <- dbTryJust err' $ dbRunQuery ins
        case i of
          Left Nothing -> do
            _ <- dbExecuteSimple "ROLLBACK TO SAVEPOINT pre_insert"
            uoi
          Left (Just e) -> return $ Left e
          Right r -> return $ Right r
      _ -> return u

updateOrInsert :: (MonadDB c m, PGQuery q a) => q -> q -> m (Int, [a])
-- updateOrInsert upd ins = either fail return <$> tryUpdateOrInsert (const Nothing) upd ins
updateOrInsert upd ins = dbTransaction uoi where
  uoi = do
    u@(n, _) <- dbRunQuery upd
    if n /= 0
      then return u
      else do
        _ <- dbExecuteSimple "SAVEPOINT pre_insert"
        i <- dbTryJust (guard . isUniqueViolation) $ dbRunQuery ins
        either (\() -> do
          _ <- dbExecuteSimple "ROLLBACK TO SAVEPOINT pre_insert"
          uoi)
          return i
