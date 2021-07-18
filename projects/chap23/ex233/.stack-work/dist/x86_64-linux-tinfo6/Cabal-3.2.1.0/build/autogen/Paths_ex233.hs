{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_ex233 (
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

bindir     = "/mnt/c/Users/Max/src/haskellbook/projects/chap23/ex233/.stack-work/install/x86_64-linux-tinfo6/8b689291ea1601ad068a48769a8010c3652d71f7bbb7af97c76a99ed0d8e5d4b/8.10.4/bin"
libdir     = "/mnt/c/Users/Max/src/haskellbook/projects/chap23/ex233/.stack-work/install/x86_64-linux-tinfo6/8b689291ea1601ad068a48769a8010c3652d71f7bbb7af97c76a99ed0d8e5d4b/8.10.4/lib/x86_64-linux-ghc-8.10.4/ex233-0.1.0.0-EfLLDXLCkuB6ho9rpJbsCp"
dynlibdir  = "/mnt/c/Users/Max/src/haskellbook/projects/chap23/ex233/.stack-work/install/x86_64-linux-tinfo6/8b689291ea1601ad068a48769a8010c3652d71f7bbb7af97c76a99ed0d8e5d4b/8.10.4/lib/x86_64-linux-ghc-8.10.4"
datadir    = "/mnt/c/Users/Max/src/haskellbook/projects/chap23/ex233/.stack-work/install/x86_64-linux-tinfo6/8b689291ea1601ad068a48769a8010c3652d71f7bbb7af97c76a99ed0d8e5d4b/8.10.4/share/x86_64-linux-ghc-8.10.4/ex233-0.1.0.0"
libexecdir = "/mnt/c/Users/Max/src/haskellbook/projects/chap23/ex233/.stack-work/install/x86_64-linux-tinfo6/8b689291ea1601ad068a48769a8010c3652d71f7bbb7af97c76a99ed0d8e5d4b/8.10.4/libexec/x86_64-linux-ghc-8.10.4/ex233-0.1.0.0"
sysconfdir = "/mnt/c/Users/Max/src/haskellbook/projects/chap23/ex233/.stack-work/install/x86_64-linux-tinfo6/8b689291ea1601ad068a48769a8010c3652d71f7bbb7af97c76a99ed0d8e5d4b/8.10.4/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "ex233_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "ex233_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "ex233_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "ex233_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "ex233_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "ex233_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
