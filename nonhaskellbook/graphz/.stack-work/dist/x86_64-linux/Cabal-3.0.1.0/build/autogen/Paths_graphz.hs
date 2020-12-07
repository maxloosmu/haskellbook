{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_graphz (
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

bindir     = "/mnt/c/Users/Max/src/haskellbook/nonhaskellbook/graphz/.stack-work/install/x86_64-linux/5563ec6afccb7741263cd5393183d9ccb731a657d0741f9a5300e32078b1ef84/8.8.4/bin"
libdir     = "/mnt/c/Users/Max/src/haskellbook/nonhaskellbook/graphz/.stack-work/install/x86_64-linux/5563ec6afccb7741263cd5393183d9ccb731a657d0741f9a5300e32078b1ef84/8.8.4/lib/x86_64-linux-ghc-8.8.4/graphz-0.1.0.0-KJsjUQRZayYLPXJfBlGDBY"
dynlibdir  = "/mnt/c/Users/Max/src/haskellbook/nonhaskellbook/graphz/.stack-work/install/x86_64-linux/5563ec6afccb7741263cd5393183d9ccb731a657d0741f9a5300e32078b1ef84/8.8.4/lib/x86_64-linux-ghc-8.8.4"
datadir    = "/mnt/c/Users/Max/src/haskellbook/nonhaskellbook/graphz/.stack-work/install/x86_64-linux/5563ec6afccb7741263cd5393183d9ccb731a657d0741f9a5300e32078b1ef84/8.8.4/share/x86_64-linux-ghc-8.8.4/graphz-0.1.0.0"
libexecdir = "/mnt/c/Users/Max/src/haskellbook/nonhaskellbook/graphz/.stack-work/install/x86_64-linux/5563ec6afccb7741263cd5393183d9ccb731a657d0741f9a5300e32078b1ef84/8.8.4/libexec/x86_64-linux-ghc-8.8.4/graphz-0.1.0.0"
sysconfdir = "/mnt/c/Users/Max/src/haskellbook/nonhaskellbook/graphz/.stack-work/install/x86_64-linux/5563ec6afccb7741263cd5393183d9ccb731a657d0741f9a5300e32078b1ef84/8.8.4/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "graphz_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "graphz_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "graphz_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "graphz_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "graphz_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "graphz_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
