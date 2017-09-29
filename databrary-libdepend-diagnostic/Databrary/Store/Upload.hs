module Databrary.Store.Upload
  ( uploadFile
  ) where

import System.Posix.FilePath (RawFilePath, (</>))

import Databrary.Has (view)
import Databrary.Store.Types
import Databrary.Model.Id
import Databrary.Model.Token.Types

uploadFile :: Upload -> Storage -> RawFilePath
uploadFile t s = storageUpload s </> unId (view t :: Id Token)
