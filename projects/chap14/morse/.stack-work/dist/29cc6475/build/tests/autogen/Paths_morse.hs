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

bindir     = "C:\\Users\\Max\\src\\haskellbook\\projects\\morse\\.stack-work\\install\\afc371a8\\bin"
libdir     = "C:\\Users\\Max\\src\\haskellbook\\projects\\morse\\.stack-work\\install\\afc371a8\\lib\\x86_64-windows-ghc-8.8.4\\morse-0.1.0.0-K5I1SYXqv7oCQrY4RclR5A-tests"
dynlibdir  = "C:\\Users\\Max\\src\\haskellbook\\projects\\morse\\.stack-work\\install\\afc371a8\\lib\\x86_64-windows-ghc-8.8.4"
datadir    = "C:\\Users\\Max\\src\\haskellbook\\projects\\morse\\.stack-work\\install\\afc371a8\\share\\x86_64-windows-ghc-8.8.4\\morse-0.1.0.0"
libexecdir = "C:\\Users\\Max\\src\\haskellbook\\projects\\morse\\.stack-work\\install\\afc371a8\\libexec\\x86_64-windows-ghc-8.8.4\\morse-0.1.0.0"
sysconfdir = "C:\\Users\\Max\\src\\haskellbook\\projects\\morse\\.stack-work\\install\\afc371a8\\etc"

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
  return (dir ++ "\\" ++ name)
