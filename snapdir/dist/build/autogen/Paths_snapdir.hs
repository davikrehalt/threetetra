module Paths_snapdir (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch

version :: Version
version = Version [0,1] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/andy/.cabal/bin"
libdir     = "/home/andy/.cabal/lib/i386-linux-ghc-7.10.1/snapd_1S8AZvoUKzVJ82UjW4oJrZ"
datadir    = "/home/andy/.cabal/share/i386-linux-ghc-7.10.1/snapdir-0.1"
libexecdir = "/home/andy/.cabal/libexec"
sysconfdir = "/home/andy/.cabal/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "snapdir_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "snapdir_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "snapdir_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "snapdir_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "snapdir_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
