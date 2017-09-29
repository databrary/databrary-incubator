{-
this is basically a setup script. it's run during installation/compilation by cabal.
Cabal has Simple and Custom setups (maybe more). This uses the deprecated Custom setup, 
and performs typical shell script tasks hooking into phases of the Haskell build. Custom setups
can interfere with stack and other build tools.
-}

import Control.Monad (when)
import qualified Data.Foldable as Fold
import Distribution.Compat.Environment (getEnvironment)
import Distribution.PackageDescription (PackageDescription(dataDir), FlagName(FlagName))
import Distribution.Simple
import Distribution.Simple.Build.PathsModule (pkgPathEnvVar)
import Distribution.Simple.BuildPaths (exeExtension)
import Distribution.Simple.InstallDirs (toPathTemplate, fromPathTemplate, datadir)
import Distribution.Simple.LocalBuildInfo (LocalBuildInfo(buildDir), absoluteInstallDirs)
import Distribution.Simple.Setup
import Distribution.Simple.Utils (notice, rawSystemExitWithEnv, setFileExecutable)
import Distribution.Verbosity (Verbosity)
import System.Directory (getCurrentDirectory)
import System.FilePath ((<.>), (</>))

-- node and git are scripted through haskell. the node scripting is 
-- basically a handrolled webpack
import Databrary.Setup.Git
import Databrary.Setup.Node

run :: Verbosity -> PackageDescription -> LocalBuildInfo -> String -> [String] -> IO ()
run verb desc lbi cmd args = do
  env <- getEnvironment
  cwd <- getCurrentDirectory
  rawSystemExitWithEnv verb (buildDir lbi </> cmd </> cmd <.> exeExtension) args
    $ (pkgPathEnvVar desc "datadir", cwd </> dataDir desc)
    : (pkgPathEnvVar desc "sysconfdir", cwd)
    : env

-- chmod +x on transctl.sh and transcode scripts
fixPerms :: PackageDescription -> LocalBuildInfo -> CopyDest -> IO ()
fixPerms desc lbi copy = do
  setFileExecutable (dir </> "transctl.sh")
  setFileExecutable (dir </> "transcode")
  where dir = datadir $ absoluteInstallDirs desc lbi copy

main :: IO ()
{- defaultMainWithHooks does the default build things but also
   takes a struct (simpleUserHooks) with hooks to be executed at particular points in the build
   setup process.
-}
main = defaultMainWithHooks simpleUserHooks 
  -- hook in node and npm and the list of default hooked programs
  { hookedPrograms = [nodeProgram, npmProgram] ++ hookedPrograms simpleUserHooks
    -- at the config step get the library version from git and check for the -devel flag
  , confHook = \(d, i) f -> do
    d' <- setGitVersion d
    let f' | Fold.or (lookup (FlagName "devel") (configConfigurationsFlags f)) || Fold.any (not . null . fromPathTemplate) (flagToMaybe $ configProgSuffix f) = f
           | otherwise = f{ configProgSuffix = Flag $ toPathTemplate "-$version" }
    confHook simpleUserHooks (d', i) f'
    -- update all node dependencies after configuring
  , postConf = \args flag desc lbi -> do
    postConf simpleUserHooks args flag desc lbi
    nodeUpdate (fromFlag $ configVerbosity flag) lbi
    {- do the webpacky thing (minifying all js) then install 
       the postgres schema (that's what the schemabrary module does)
    -}
  , buildHook = \desc lbi hooks flag -> do
    let verb = fromFlag $ buildVerbosity flag
    nodeModuleGenerate verb desc lbi
    let args = buildArgs flag
        build c = buildHook simpleUserHooks desc lbi hooks flag{ buildArgs = c }
    when (null args) $ do
      build ["schemabrary"]
      run verb desc lbi "schemabrary" []
    build args
    {- after build bundle all the web assets and js (that's the -w flag)
    -}
  , postBuild = \args flag desc lbi -> do
    let verb = fromFlag $ buildVerbosity flag
    notice verb "Generating web ..."
    run verb desc lbi "databrary" ["-w"]
    postBuild simpleUserHooks args flag desc lbi
    -- do the chmod +x thing to transcode and transctl.sh
  , postCopy = \args flag desc lbi -> do
    fixPerms desc lbi (fromFlag $ copyDest flag)
    postCopy simpleUserHooks args flag desc lbi
  , postInst = \args flag desc lbi -> do
    fixPerms desc lbi NoCopyDest
    postInst simpleUserHooks args flag desc lbi
  }
