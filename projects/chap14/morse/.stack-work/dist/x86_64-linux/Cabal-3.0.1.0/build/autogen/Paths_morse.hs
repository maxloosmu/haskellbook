{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_morse (
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

bindir     = "/mnt/c/Users/Max/src/haskellbook/projects/morse/.stack-work/install/x86_64-linux/bab9d063b8d7337cbd3ed203e0cacc9cbabcbf77822e10e16bcf3f98b9b1c49e/8.8.4/bin"
libdir     = "/mnt/c/Users/Max/src/haskellbook/projects/morse/.stack-work/install/x86_64-linux/bab9d063b8d7337cbd3ed203e0cacc9cbabcbf77822e10e16bcf3f98b9b1c49e/8.8.4/lib/x86_64-linux-ghc-8.8.4/morse-0.1.0.0-50nSuV4HJzpLL7lYEll2Is"
dynlibdir  = "/mnt/c/Users/Max/src/haskellbook/projects/morse/.stack-work/install/x86_64-linux/bab9d063b8d7337cbd3ed203e0cacc9cbabcbf77822e10e16bcf3f98b9b1c49e/8.8.4/lib/x86_64-linux-ghc-8.8.4"
datadir    = "/mnt/c/Users/Max/src/haskellbook/projects/morse/.stack-work/install/x86_64-linux/bab9d063b8d7337cbd3ed203e0cacc9cbabcbf77822e10e16bcf3f98b9b1c49e/8.8.4/share/x86_64-linux-ghc-8.8.4/morse-0.1.0.0"
libexecdir = "/mnt/c/Users/Max/src/haskellbook/projects/morse/.stack-work/install/x86_64-linux/bab9d063b8d7337cbd3ed203e0cacc9cbabcbf77822e10e16bcf3f98b9b1c49e/8.8.4/libexec/x86_64-linux-ghc-8.8.4/morse-0.1.0.0"
sysconfdir = "/mnt/c/Users/Max/src/haskellbook/projects/morse/.stack-work/install/x86_64-linux/bab9d063b8d7337cbd3ed203e0cacc9cbabcbf77822e10e16bcf3f98b9b1c49e/8.8.4/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "morse_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "morse_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "morse_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "morse_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "morse_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "morse_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
