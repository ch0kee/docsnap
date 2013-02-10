module Paths_DocSnap (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch


version :: Version
version = Version {versionBranch = [0,1,2], versionTags = []}
bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/home/ch0kee/.cabal/bin"
libdir     = "/home/ch0kee/.cabal/lib/DocSnap-0.1.2/ghc-7.4.2"
datadir    = "/home/ch0kee/.cabal/share/DocSnap-0.1.2"
libexecdir = "/home/ch0kee/.cabal/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catchIO (getEnv "DocSnap_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "DocSnap_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "DocSnap_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "DocSnap_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
