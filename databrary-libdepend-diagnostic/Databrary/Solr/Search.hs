{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Databrary.Solr.Search
  ( SearchType(..)
  , SearchQuery(..)
  , search
  ) where

import Control.Arrow (first)
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import Data.Char (isAlphaNum)
import Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Network.HTTP.Client as HC
import Network.HTTP.Types.URI (renderSimpleQuery)

import Data.ByteString.Builder.Escape (escapeLazyByteStringCharsWith, escapeTextWith)
import Databrary.Has
import Databrary.Model.Paginate
import Databrary.Model.Metric.Types
import Databrary.Solr.Service
import Databrary.Solr.Document

data SearchType
  = SearchVolumes
  | SearchParties
  deriving (Eq)

data SearchQuery = SearchQuery
  { searchString :: Maybe T.Text
  , searchFields :: [(T.Text, T.Text)]
  , searchMetrics :: [(Metric, T.Text)]
  , searchType :: SearchType
  , searchPaginate :: !Paginate
  }

checkTerm :: T.Text -> Bool
checkTerm = cq False [] . T.unpack where
  cq False [] "" = True
  cq q g ('"':s) = cq (not q) g s
  cq q g ('\\':_:s) = cq q g s
  cq _ _ ['\\'] = False
  cq False ('(':gs) (')':s) = cq False gs s
  cq False ('[':gs) (']':s) = cq False gs s
  cq False ('[':gs) ('}':s) = cq False gs s
  cq False ('{':gs) (']':s) = cq False gs s
  cq False ('{':gs) ('}':s) = cq False gs s
  cq False g (c:s)
    | c `elem` ['(','[','{'] = cq False (c:g) s
    | c `elem` [')',']','}'] = False
  cq q g (_:s) = cq q g s
  cq _ _ _ = False

checkField :: T.Text -> Bool
checkField = T.all cc where
  cc '_' = True
  cc c = isAlphaNum c

quoteQuery :: (Char -> String -> a -> B.Builder) -> a -> B.Builder
quoteQuery e s = B.char8 '"' <> e '\\' "\"\\" s <> B.char8 '"'

defaultParams :: B.Builder
defaultParams = B.string8 "{!dismax qf=\"text_en^0.5 text_gen^0.5 keyword^2 tag_name party_name\" pf=\"keyword^2 tag_name party_name\" ps=3}"

search :: MonadSolr c m => SearchQuery -> m (HC.Response BSL.ByteString)
search SearchQuery{..} = do
  req <- peeks solrRequest
  focusIO $ HC.httpLbs req
    { HC.path = HC.path req <> "search"
    , HC.queryString = renderSimpleQuery True query
    , HC.checkStatus = \_ _ _ -> Nothing
    }
  where
  query =
    [ ("q", BSL.toStrict $ B.toLazyByteString $ qp <> uw ql)
    , ("fq", "content_type:" <> ct)
    , ("start", BSC.pack $ show $ paginateOffset searchPaginate)
    , ("rows", BSC.pack $ show $ paginateLimit searchPaginate)
    , ("q.op", "AND")
    ]
    ++ maybe [] (\q ->
      [ ("spellcheck", "true")
      , ("spellcheck.q", TE.encodeUtf8 q)
      ]) searchString
  (ct, qp, qe) = case searchType of
    SearchVolumes -> ("volume", mempty, B.string8 "{!join from=volume_id to=volume_id}")
    SearchParties -> ("party", mempty, mempty)
  ql = maybe id ((:) . bp . (defaultParams <>) . TE.encodeUtf8Builder) searchString $
    map bt (searchFields ++ map (first metricField) searchMetrics)
  bt (f, v)
    | checkField f && checkTerm v = bp (TE.encodeUtf8Builder f <> B.string8 ":(" <> (if T.null v then B.char8 '*' else TE.encodeUtf8Builder v) <> B.char8 ')')
    | otherwise = bp (B.string8 "{!dismax qf=" <> quoteQuery escapeTextWith f <> B.char8 '}' <> TE.encodeUtf8Builder v)
  bp v = B.string8 "_query_:" <> quoteQuery escapeLazyByteStringCharsWith (B.toLazyByteString $ qe <> v)
  uw [] = B.string8 "*:*"
  uw (t:l) = t <> foldMap (B.char8 ' ' <>) l
