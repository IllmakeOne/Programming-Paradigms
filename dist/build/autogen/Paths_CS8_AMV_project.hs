{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_CS8_AMV_project (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "C:\\Users\\Robi\\AppData\\Roaming\\cabal\\bin"
libdir     = "C:\\Users\\Robi\\AppData\\Roaming\\cabal\\x86_64-windows-ghc-8.0.2\\CS8-AMV-project-0.1.0.0-86K0HxtihdJ8spzhYQ4RWN"
dynlibdir  = "C:\\Users\\Robi\\AppData\\Roaming\\cabal\\x86_64-windows-ghc-8.0.2"
datadir    = "C:\\Users\\Robi\\AppData\\Roaming\\cabal\\x86_64-windows-ghc-8.0.2\\CS8-AMV-project-0.1.0.0"
libexecdir = "C:\\Users\\Robi\\AppData\\Roaming\\cabal\\CS8-AMV-project-0.1.0.0-86K0HxtihdJ8spzhYQ4RWN"
sysconfdir = "C:\\Users\\Robi\\AppData\\Roaming\\cabal\\etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "CS8_AMV_project_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "CS8_AMV_project_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "CS8_AMV_project_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "CS8_AMV_project_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "CS8_AMV_project_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "CS8_AMV_project_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
