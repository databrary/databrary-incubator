{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Databrary.Service.Messages
  ( Messages
  , messagesFile
  , loadMessagesFrom
  , loadMessages
  , getMessage
  ) where

import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import Paths_databrary (getDataFileName)
import qualified Databrary.Store.Config as C
import qualified Databrary.JSON as JSON

newtype Messages = Messages C.Config
  deriving (JSON.ToJSON)

messagesFile :: IO FilePath
messagesFile = getDataFileName "messages.conf"

loadMessagesFrom :: FilePath -> IO Messages
loadMessagesFrom f = Messages <$> C.load f

loadMessages :: IO Messages
loadMessages = loadMessagesFrom =<< messagesFile

getMessage :: C.Path -> Messages -> T.Text
getMessage p (Messages c) = fromMaybe ('[' `T.cons` TE.decodeLatin1 (C.pathKey p) `T.snoc` ']') $ c C.! p
