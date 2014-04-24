module Paths_term (
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
version = Version {versionBranch = [0,1,0,0], versionTags = []}
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/nona/.cabal/bin"
libdir     = "/home/nona/.cabal/lib/x86_64-linux-ghc-7.6.3/term-0.1.0.0"
datadir    = "/home/nona/.cabal/share/x86_64-linux-ghc-7.6.3/term-0.1.0.0"
libexecdir = "/home/nona/.cabal/libexec"
sysconfdir = "/home/nona/.cabal/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "term_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "term_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "term_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "term_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "term_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
