{-# LANGUAGE OverloadedStrings, ViewPatterns, TupleSections #-}
module Databrary.Web.Rules
  ( generateWebFile
  , generateWebFiles
  ) where

import Control.Monad (guard, mzero, msum)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State.Strict (execStateT, modify, gets)
import Control.Monad.Trans.Except (runExceptT, withExceptT)
import qualified Data.HashMap.Strict as HM

import Databrary.Ops
import Databrary.Files
import Databrary.Web
import Databrary.Web.Types
import Databrary.Web.Files
import Databrary.Web.Info
import Databrary.Web.Generate
import Databrary.Web.Constants
import Databrary.Web.Routes
import Databrary.Web.Templates
import Databrary.Web.Messages
import Databrary.Web.Coffee
import Databrary.Web.Uglify
import Databrary.Web.Stylus
import Databrary.Web.Libs
import Databrary.Web.JSHint
import Databrary.Web.All
import Databrary.Web.GZip

staticGenerators :: [(FilePath, WebGenerator)]
staticGenerators =
  [ ("constants.json", generateConstantsJSON)
  , ("constants.js",   generateConstantsJS)
  , ("routes.js",      generateRoutesJS)
  ]

fixedGenerators :: [(FilePath, WebGenerator)]
fixedGenerators =
  [ ("messages.js",    generateMessagesJS)
  , ("templates.js",   generateTemplatesJS)
  , ("app.min.js",     generateUglifyJS)
  , ("app.css",        generateStylusCSS)
  , ("app.min.css",    generateStylusCSS)
  , ("all.min.js",     generateAllJS)
  , ("all.min.css",    generateAllCSS)
  ]

generateFixed :: Bool -> WebGenerator
generateFixed a fo@(f, _)
  | Just g <- lookup (webFileRel f) $ (if a then (staticGenerators ++) else id) fixedGenerators = g fo
  | otherwise = mzero

generateStatic :: WebGenerator
generateStatic fo@(f, _) = fileNewer f fo

generateRules :: Bool -> WebGenerator
generateRules a f = msum $ map ($ f)
  [ generateFixed a
  , generateCoffeeJS
  , generateLib
  , checkJSHint
  , generateGZip
  , generateStatic
  ]

updateWebInfo :: WebFilePath -> WebGeneratorM WebFileInfo
updateWebInfo f = do
  n <- liftIO $ makeWebFileInfo f
  modify $ HM.insert f n
  return n

generateWebFile :: Bool -> WebFilePath -> WebGeneratorM WebFileInfo
generateWebFile a f = withExceptT (label (webFileRel f)) $ do
  o <- gets $ HM.lookup f
  r <- generateRules a (f, o)
  fromMaybeM (updateWebInfo f) (guard (not r) >> o)
  where
  label n "" = n
  label n s = n ++ ": " ++ s

generateAll :: WebGeneratorM ()
generateAll = do
  svg <- liftIO $ findWebFiles ".svg"
  mapM_ (generateWebFile True) $
    (map (fromFilePath . fst) staticGenerators) ++
    ["constants.json.gz", "all.min.js.gz", "all.min.css.gz"] ++
    map (<.> ".gz") svg

generateWebFiles :: IO WebFileMap
generateWebFiles = do
  execStateT (either fail return =<< runExceptT generateAll) HM.empty
