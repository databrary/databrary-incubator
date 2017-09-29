module Databrary.Store.Stage
  ( stageFile
  ) where

import Databrary.Ops
import Databrary.Files
import Databrary.Store.Types

stageFile :: IsFilePath f => f -> Storage -> IO (Maybe f)
stageFile f Storage{ storageStage = Just s } =
  (sf <?) <$> fileExist sf where sf = fromRawFilePath s </> f
stageFile _ _ = return Nothing
