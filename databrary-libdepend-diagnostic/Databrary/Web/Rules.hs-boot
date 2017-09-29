module Databrary.Web.Rules where

import Databrary.Web (WebFilePath)
import Databrary.Web.Types (WebGeneratorM, WebFileInfo)

generateWebFile :: Bool -> WebFilePath -> WebGeneratorM WebFileInfo
