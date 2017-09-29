{-# LANGUAGE OverloadedStrings #-}
module Databrary.Controller.Analytics
  ( angularAnalytics
  ) where

import Control.Monad (when)
import qualified Data.Attoparsec.ByteString as P
import qualified Data.HashMap.Strict as HM
import Data.Maybe (mapMaybe, maybeToList)
import qualified Data.Vector as V

import Databrary.Has (peek)
import qualified Databrary.JSON as JSON
import Databrary.Model.Audit
import Databrary.HTTP.Request
import Databrary.Action.Request

angularAnalytics :: MonadAudit q m => m ()
angularAnalytics = do
  req <- peek
  when (isDatabraryClient req) $
    mapM_ auditAnalytic $ pr . P.parseOnly JSON.json' =<< lookupRequestHeaders "analytics" req
  where
  pr (Left _) = []
  pr (Right (JSON.Array l)) = mapMaybe ar $ V.toList l
  pr (Right j) = maybeToList $ ar j
  ar (JSON.Object o) = Analytic
    <$> (JSON.parseMaybe JSON.parseJSON =<< HM.lookup "action" o)
    <*> (JSON.parseMaybe JSON.parseJSON =<< HM.lookup "route" o)
    <*> return (HM.lookup "data" o)
  ar _ = Nothing
