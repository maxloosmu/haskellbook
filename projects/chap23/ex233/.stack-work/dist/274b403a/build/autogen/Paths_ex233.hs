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

bindir     = "C:\\Users\\Max\\src\\haskellbook\\projects\\chap23\\ex233\\.stack-work\\install\\2ad1fb84\\bin"
libdir     = "C:\\Users\\Max\\src\\haskellbook\\projects\\chap23\\ex233\\.stack-work\\install\\2ad1fb84\\lib\\x86_64-windows-ghc-8.10.4\\ex233-0.1.0.0-7Gz8sLtQMry1A3SYjr4UhY"
dynlibdir  = "C:\\Users\\Max\\src\\haskellbook\\projects\\chap23\\ex233\\.stack-work\\install\\2ad1fb84\\lib\\x86_64-windows-ghc-8.10.4"
datadir    = "C:\\Users\\Max\\src\\haskellbook\\projects\\chap23\\ex233\\.stack-work\\install\\2ad1fb84\\share\\x86_64-windows-ghc-8.10.4\\ex233-0.1.0.0"
libexecdir = "C:\\Users\\Max\\src\\haskellbook\\projects\\chap23\\ex233\\.stack-work\\install\\2ad1fb84\\libexec\\x86_64-windows-ghc-8.10.4\\ex233-0.1.0.0"
sysconfdir = "C:\\Users\\Max\\src\\haskellbook\\projects\\chap23\\ex233\\.stack-work\\install\\2ad1fb84\\etc"

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
  return (dir ++ "\\" ++ name)
