{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_ex184 (
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

bindir     = "C:\\Users\\Max\\src\\haskellbook\\projects\\chap18\\ex184\\.stack-work\\install\\a77f9832\\bin"
libdir     = "C:\\Users\\Max\\src\\haskellbook\\projects\\chap18\\ex184\\.stack-work\\install\\a77f9832\\lib\\x86_64-windows-ghc-8.8.4\\ex184-0.1.0.0-CxBtKFdciT77YRMZrnZTf7"
dynlibdir  = "C:\\Users\\Max\\src\\haskellbook\\projects\\chap18\\ex184\\.stack-work\\install\\a77f9832\\lib\\x86_64-windows-ghc-8.8.4"
datadir    = "C:\\Users\\Max\\src\\haskellbook\\projects\\chap18\\ex184\\.stack-work\\install\\a77f9832\\share\\x86_64-windows-ghc-8.8.4\\ex184-0.1.0.0"
libexecdir = "C:\\Users\\Max\\src\\haskellbook\\projects\\chap18\\ex184\\.stack-work\\install\\a77f9832\\libexec\\x86_64-windows-ghc-8.8.4\\ex184-0.1.0.0"
sysconfdir = "C:\\Users\\Max\\src\\haskellbook\\projects\\chap18\\ex184\\.stack-work\\install\\a77f9832\\etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "ex184_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "ex184_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "ex184_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "ex184_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "ex184_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "ex184_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
