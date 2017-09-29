{-# LANGUAGE OverloadedStrings #-}
module Databrary.Model.Paginate
  ( Paginate(..)
  , paginateSQL
  , def
  ) where

import qualified Data.ByteString as BS
import Data.Default.Class (Default(..))
import Data.Int (Int32)
import Data.Monoid ((<>))
import Database.PostgreSQL.Typed.Dynamic (pgLiteralRep)

data Paginate = Paginate
  { paginateOffset, paginateLimit :: !Int32
  } deriving (Eq, Ord)

instance Bounded Paginate where
  minBound = Paginate 0 1
  maxBound = Paginate maxBound 129

instance Default Paginate where
  def = Paginate 0 25

instance Enum Paginate where
  succ (Paginate o l) = Paginate (o+l) l
  pred (Paginate 0 _) = error "pred Paginate: invalid argument"
  pred (Paginate o l) = Paginate (o-l `max` 0) l
  toEnum i = Paginate (d*fromIntegral i) d where d = paginateLimit def
  fromEnum (Paginate o l) = fromIntegral $ o + l - 1 `div` l

paginateSQL :: Paginate -> BS.ByteString
paginateSQL (Paginate o l) = "LIMIT " <> pgLiteralRep l <> " OFFSET " <> pgLiteralRep o
