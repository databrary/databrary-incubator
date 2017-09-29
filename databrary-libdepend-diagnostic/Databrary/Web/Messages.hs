{-# LANGUAGE OverloadedStrings #-}
module Databrary.Web.Messages
  ( generateMessagesJS
  ) where

import Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson.Encode as JSON
import qualified Data.ByteString.Builder as BSB
import System.IO (withBinaryFile, IOMode(WriteMode), hPutStr)

import qualified Databrary.JSON as JSON
import Databrary.Service.Messages
import Databrary.Web
import Databrary.Web.Types
import Databrary.Web.Generate

generateMessagesJS :: WebGenerator
generateMessagesJS fo@(f, _) = do
  mf <- liftIO messagesFile
  webRegenerate (do
    msg <- liftIO $ loadMessagesFrom mf
    withBinaryFile (webFileAbs f) WriteMode $ \h -> do
      hPutStr h "app.constant('messageData',"
      BSB.hPutBuilder h $ JSON.encodeToBuilder $ JSON.toJSON msg
      hPutStr h ");")
    [mf] [] fo
