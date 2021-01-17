{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_code (
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

bindir     = "/mnt/c/Users/Max/src/haskellbook/projects/chap17/code/.stack-work/install/x86_64-linux/922769dcddf88c96dc28f217033243d5a4be75b540ddb1213bd2c81d0b2bb902/8.8.4/bin"
libdir     = "/mnt/c/Users/Max/src/haskellbook/projects/chap17/code/.stack-work/install/x86_64-linux/922769dcddf88c96dc28f217033243d5a4be75b540ddb1213bd2c81d0b2bb902/8.8.4/lib/x86_64-linux-ghc-8.8.4/code-0.1.0.0-GhgualUfbhvHrifFPgrOaO"
dynlibdir  = "/mnt/c/Users/Max/src/haskellbook/projects/chap17/code/.stack-work/install/x86_64-linux/922769dcddf88c96dc28f217033243d5a4be75b540ddb1213bd2c81d0b2bb902/8.8.4/lib/x86_64-linux-ghc-8.8.4"
datadir    = "/mnt/c/Users/Max/src/haskellbook/projects/chap17/code/.stack-work/install/x86_64-linux/922769dcddf88c96dc28f217033243d5a4be75b540ddb1213bd2c81d0b2bb902/8.8.4/share/x86_64-linux-ghc-8.8.4/code-0.1.0.0"
libexecdir = "/mnt/c/Users/Max/src/haskellbook/projects/chap17/code/.stack-work/install/x86_64-linux/922769dcddf88c96dc28f217033243d5a4be75b540ddb1213bd2c81d0b2bb902/8.8.4/libexec/x86_64-linux-ghc-8.8.4/code-0.1.0.0"
sysconfdir = "/mnt/c/Users/Max/src/haskellbook/projects/chap17/code/.stack-work/install/x86_64-linux/922769dcddf88c96dc28f217033243d5a4be75b540ddb1213bd2c81d0b2bb902/8.8.4/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "code_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "code_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "code_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "code_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "code_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "code_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
