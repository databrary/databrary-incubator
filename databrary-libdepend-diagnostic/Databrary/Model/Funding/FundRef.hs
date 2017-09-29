{-# LANGUAGE OverloadedStrings #-}
module Databrary.Model.Funding.FundRef
  ( fundRefDOI
  , lookupFunderRef
  , searchFundRef
  ) where

import Control.Monad ((<=<), (>=>), guard, mfilter)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import Data.Foldable (fold)
import Data.Function (on)
import qualified Data.HashMap.Strict as HM
import Data.List (stripPrefix, sortBy, nubBy)
import Data.Maybe (fromJust, mapMaybe)
import Data.Monoid ((<>))
import Data.Ord (comparing, Down(..))
import Data.String (IsString(..))
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import qualified Network.HTTP.Client as HC
import Text.Read (readMaybe)

import Databrary.Ops
import Databrary.Has (MonadHas, focusIO)
import qualified Databrary.JSON as JSON
import Databrary.HTTP.Client
import Databrary.Service.DB
import Databrary.Model.Id.Types
import Databrary.Model.GeoNames
import Databrary.Model.Funding

fundRefDOI :: String
fundRefDOI = "10.13039/"

-- Not quite like Data.CaseInsensitive (non-strict 'ciFolded')
data CI = CI
  { unCI :: !T.Text
  , ciFolded :: T.Text
  }

{-# NOINLINE toCI #-} -- toCaseFold's entire switch gets inlined!
toCI :: T.Text -> CI
toCI t = CI t (T.toCaseFold t)

instance IsString CI where
  fromString = toCI . fromString

onCI :: (T.Text -> T.Text -> a) -> CI -> CI -> a
onCI f = f `on` ciFolded

instance Eq CI where
  (==) = onCI (==)
  (/=) = onCI (/=)

annotateFunder :: Funder -> [T.Text] -> Maybe T.Text -> Funder
annotateFunder f [] Nothing = f
annotateFunder f@Funder{ funderName = n } a c = f{ funderName =
  maybe id (\cc -> (<> (", " <> cc))) (mfilter (not . noc . toCI) c)
  $ case unCI <$> nai' of
      [] -> "" -- impossible
      [nn] -> nn
      (nn:aa) -> nn <> " (" <> T.intercalate ", " aa <> ")"
  }
  where
  ni = toCI n
  ai = toCI <$> sortBy (comparing $ Down . T.length) a
  nai' = nubBy (onCI T.isInfixOf) $
    (case filter (T.isInfixOf `onCI` ni) ai of
      [] -> ni
      (lni:_) -> lni) : ai
  noc ci = toCI (geoName geoNameUS) == ci || any (T.isInfixOf `onCI` ci) nai'

parseFundRef :: JSON.Value -> JSON.Parser (Funder, Maybe (Id GeoName))
parseFundRef = JSON.withObject "fundref" $ \j -> do
  doi <- j JSON..: "id"
  fid <- maybe (fail $ "doi: " ++ doi) (return . Id) $ readMaybe =<< stripPrefix ("http://dx.doi.org/" ++ fundRefDOI) doi
  name <- label =<< j JSON..: "prefLabel"
  let alts = mapMaybe (JSON.parseMaybe (label <=< JSON.parseJSON)) $ case HM.lookup "altLabel" j of
        Just (JSON.Array v) -> V.toList v
        Just o -> [o]
        Nothing -> []
      geo = do
        r <- JSON.parseMaybe ((JSON..: "country") >=> (JSON..: "resource")) j
        g <- parseGeoNameRef r
        guard (g /= geoNameId geoNameUS)
        return g
  return (annotateFunder (Funder fid name) alts Nothing, geo)
  where
  label j = j JSON..: "Label" >>= (JSON..: "literalForm") >>= (JSON..: "content")

lookupFundRef :: Id Funder -> HTTPClient -> IO (Maybe Funder)
lookupFundRef fi hcm = runMaybeT $ do
  req <- HC.parseUrl $ "http://data.fundref.org/fundref/funder/" ++ fundRefDOI ++ show fi
  j <- MaybeT $ httpRequestJSON req hcm
  (f, gi) <- MaybeT $ return $ JSON.parseMaybe parseFundRef j
  g <- lift $ flatMapM (\i -> lookupGeoName i hcm) gi
  return $ annotateFunder f [] (geoName <$> g)

lookupFunderRef :: (MonadIO m, MonadDB c m, MonadHas HTTPClient c m) => Id Funder -> m (Maybe Funder)
lookupFunderRef fi = do
  f <- lookupFunder fi
  f `orElseM` do
    r <- focusIO $ lookupFundRef fi
    mapM_ addFunder r
    return r

parseFundRefs :: JSON.Value -> JSON.Parser [Funder]
parseFundRefs = JSON.withArray "fundrefs" $
  return . mapMaybe (JSON.parseMaybe pfr) . V.toList
  where
  pfr = JSON.withObject "fundref" $ \j -> do
    is <- j JSON..: "id"
    i <- maybe (fail "invalid id") (return . Id) $ readMaybe is
    name <- j JSON..: "value"
    alts <- j JSON..:? "other_names"
    country <- j JSON..:? "country"
    return $ annotateFunder (Funder i name) (fold alts) country

fundRefReq :: HC.Request
fundRefReq = (fromJust $ HC.parseUrl "http://search.crossref.org/funders")
  { HC.cookieJar = Nothing }

searchFundRef :: T.Text -> HTTPClient -> IO [Funder]
searchFundRef q hcm = do
  j <- httpRequestJSON req hcm
  return $ fold $ JSON.parseMaybe parseFundRefs =<< j
  where req = HC.setQueryString [("q", Just $ TE.encodeUtf8 q)] fundRefReq
