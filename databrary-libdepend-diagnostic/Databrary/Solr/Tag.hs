{-# LANGUAGE OverloadedStrings #-}
module Databrary.Solr.Tag
  ( termTags
  ) where

import Control.Monad ((>=>))
import qualified Data.Aeson.Types as JSON
import qualified Data.ByteString.Char8 as BSC
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import qualified Data.Text.Encoding as TE
import qualified Network.HTTP.Client as HC
import Network.HTTP.Types.URI (renderSimpleQuery)

import Databrary.Ops
import Databrary.Has
import Databrary.Service.DB
import Databrary.HTTP.Client
import Databrary.Model.Tag
import Databrary.Solr.Service

parseTags :: JSON.Value -> JSON.Parser [TagName]
parseTags = JSON.withObject "terms" $
  (JSON..: "terms") >=> (JSON..: "tag_name") >=> tf where
  tf [] = return []
  tf (JSON.String n:JSON.Number _c:l) = (TagName (TE.encodeUtf8 n) :) <$> tf l
  tf _ = fail "mismatched terms"

termTags :: (MonadSolr c m, MonadDB c m) => Maybe TagName -> Int -> m [TagName]
termTags pre count = do
  req <- peeks solrRequest
  r <- focusIO $ httpRequestJSON req
    { HC.path = HC.path req <> "terms"
    , HC.queryString = renderSimpleQuery True $
      maybe id ((:) . (,) "terms.prefix" . tagNameBS) pre
      [ ("terms.fl", "tag_name")
      , ("terms.limit", BSC.pack $ show count)
      ]
    }
  fromMaybeM (map tagName <$> findTags (fromMaybe (TagName "") pre) count) $
    JSON.parseMaybe parseTags =<< r
