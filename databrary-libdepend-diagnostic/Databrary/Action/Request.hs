{- a single lone file for a single lone function that checks if any of the request headers
   is "DatabraryClient"
-}

{-# LANGUAGE OverloadedStrings #-}
module Databrary.Action.Request
  ( isDatabraryClient
  ) where

import qualified Network.Wai as Wai

import Databrary.HTTP.Request

isDatabraryClient :: Wai.Request -> Bool
isDatabraryClient = any ("DatabraryClient" ==) . lookupRequestHeader "x-requested-with"
