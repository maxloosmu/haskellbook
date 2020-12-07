{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_testing (
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

bindir     = "/mnt/c/Users/Max/src/haskellbook/projects/chap16/testing/.stack-work/install/x86_64-linux/040dc24331e981252b1fd419230c7299118126dfe810db0511211ab65d400acf/8.8.4/bin"
libdir     = "/mnt/c/Users/Max/src/haskellbook/projects/chap16/testing/.stack-work/install/x86_64-linux/040dc24331e981252b1fd419230c7299118126dfe810db0511211ab65d400acf/8.8.4/lib/x86_64-linux-ghc-8.8.4/testing-0.1.0.0-L8fisPxkqaiEahkxeF6VKj"
dynlibdir  = "/mnt/c/Users/Max/src/haskellbook/projects/chap16/testing/.stack-work/install/x86_64-linux/040dc24331e981252b1fd419230c7299118126dfe810db0511211ab65d400acf/8.8.4/lib/x86_64-linux-ghc-8.8.4"
datadir    = "/mnt/c/Users/Max/src/haskellbook/projects/chap16/testing/.stack-work/install/x86_64-linux/040dc24331e981252b1fd419230c7299118126dfe810db0511211ab65d400acf/8.8.4/share/x86_64-linux-ghc-8.8.4/testing-0.1.0.0"
libexecdir = "/mnt/c/Users/Max/src/haskellbook/projects/chap16/testing/.stack-work/install/x86_64-linux/040dc24331e981252b1fd419230c7299118126dfe810db0511211ab65d400acf/8.8.4/libexec/x86_64-linux-ghc-8.8.4/testing-0.1.0.0"
sysconfdir = "/mnt/c/Users/Max/src/haskellbook/projects/chap16/testing/.stack-work/install/x86_64-linux/040dc24331e981252b1fd419230c7299118126dfe810db0511211ab65d400acf/8.8.4/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "testing_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "testing_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "testing_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "testing_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "testing_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "testing_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
