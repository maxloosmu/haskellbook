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

bindir     = "C:\\Users\\Max\\src\\haskellbook\\nonhaskellbook\\graphz\\.stack-work\\install\\509d0240\\bin"
libdir     = "C:\\Users\\Max\\src\\haskellbook\\nonhaskellbook\\graphz\\.stack-work\\install\\509d0240\\lib\\x86_64-windows-ghc-8.8.4\\graphz-0.1.0.0-1bUHdJ5pAUE7P8h3X1NIba"
dynlibdir  = "C:\\Users\\Max\\src\\haskellbook\\nonhaskellbook\\graphz\\.stack-work\\install\\509d0240\\lib\\x86_64-windows-ghc-8.8.4"
datadir    = "C:\\Users\\Max\\src\\haskellbook\\nonhaskellbook\\graphz\\.stack-work\\install\\509d0240\\share\\x86_64-windows-ghc-8.8.4\\graphz-0.1.0.0"
libexecdir = "C:\\Users\\Max\\src\\haskellbook\\nonhaskellbook\\graphz\\.stack-work\\install\\509d0240\\libexec\\x86_64-windows-ghc-8.8.4\\graphz-0.1.0.0"
sysconfdir = "C:\\Users\\Max\\src\\haskellbook\\nonhaskellbook\\graphz\\.stack-work\\install\\509d0240\\etc"

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
  return (dir ++ "\\" ++ name)
