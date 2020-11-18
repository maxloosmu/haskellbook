{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_semigroup (
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

bindir     = "/mnt/c/Users/Max/src/haskellbook/projects/chap15/semigroup/.stack-work/install/x86_64-linux/1a7e75f0af209c4c6f3c02802e35e30de8ef137f3954d993d10ce3696155b5f1/8.8.4/bin"
libdir     = "/mnt/c/Users/Max/src/haskellbook/projects/chap15/semigroup/.stack-work/install/x86_64-linux/1a7e75f0af209c4c6f3c02802e35e30de8ef137f3954d993d10ce3696155b5f1/8.8.4/lib/x86_64-linux-ghc-8.8.4/semigroup-0.1.0.0-19m0qfpaOIgAPKY7KlpGI5"
dynlibdir  = "/mnt/c/Users/Max/src/haskellbook/projects/chap15/semigroup/.stack-work/install/x86_64-linux/1a7e75f0af209c4c6f3c02802e35e30de8ef137f3954d993d10ce3696155b5f1/8.8.4/lib/x86_64-linux-ghc-8.8.4"
datadir    = "/mnt/c/Users/Max/src/haskellbook/projects/chap15/semigroup/.stack-work/install/x86_64-linux/1a7e75f0af209c4c6f3c02802e35e30de8ef137f3954d993d10ce3696155b5f1/8.8.4/share/x86_64-linux-ghc-8.8.4/semigroup-0.1.0.0"
libexecdir = "/mnt/c/Users/Max/src/haskellbook/projects/chap15/semigroup/.stack-work/install/x86_64-linux/1a7e75f0af209c4c6f3c02802e35e30de8ef137f3954d993d10ce3696155b5f1/8.8.4/libexec/x86_64-linux-ghc-8.8.4/semigroup-0.1.0.0"
sysconfdir = "/mnt/c/Users/Max/src/haskellbook/projects/chap15/semigroup/.stack-work/install/x86_64-linux/1a7e75f0af209c4c6f3c02802e35e30de8ef137f3954d993d10ce3696155b5f1/8.8.4/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "semigroup_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "semigroup_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "semigroup_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "semigroup_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "semigroup_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "semigroup_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
