{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_typefam (
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

bindir     = "/mnt/c/Users/Max/src/haskellbook/nonhaskellbook/internet/serokell/typefam/.stack-work/install/x86_64-linux-tinfo6/442b15533237dc653ceed09b5b268d2ab9592b1eb23247577064ae3f60b2ac18/8.10.4/bin"
libdir     = "/mnt/c/Users/Max/src/haskellbook/nonhaskellbook/internet/serokell/typefam/.stack-work/install/x86_64-linux-tinfo6/442b15533237dc653ceed09b5b268d2ab9592b1eb23247577064ae3f60b2ac18/8.10.4/lib/x86_64-linux-ghc-8.10.4/typefam-0.1.0.0-f7RLRD8c4rLegO4GZ3RW9"
dynlibdir  = "/mnt/c/Users/Max/src/haskellbook/nonhaskellbook/internet/serokell/typefam/.stack-work/install/x86_64-linux-tinfo6/442b15533237dc653ceed09b5b268d2ab9592b1eb23247577064ae3f60b2ac18/8.10.4/lib/x86_64-linux-ghc-8.10.4"
datadir    = "/mnt/c/Users/Max/src/haskellbook/nonhaskellbook/internet/serokell/typefam/.stack-work/install/x86_64-linux-tinfo6/442b15533237dc653ceed09b5b268d2ab9592b1eb23247577064ae3f60b2ac18/8.10.4/share/x86_64-linux-ghc-8.10.4/typefam-0.1.0.0"
libexecdir = "/mnt/c/Users/Max/src/haskellbook/nonhaskellbook/internet/serokell/typefam/.stack-work/install/x86_64-linux-tinfo6/442b15533237dc653ceed09b5b268d2ab9592b1eb23247577064ae3f60b2ac18/8.10.4/libexec/x86_64-linux-ghc-8.10.4/typefam-0.1.0.0"
sysconfdir = "/mnt/c/Users/Max/src/haskellbook/nonhaskellbook/internet/serokell/typefam/.stack-work/install/x86_64-linux-tinfo6/442b15533237dc653ceed09b5b268d2ab9592b1eb23247577064ae3f60b2ac18/8.10.4/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "typefam_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "typefam_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "typefam_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "typefam_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "typefam_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "typefam_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
