module Paths_Reserv (
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
version = Version [0,1,0,0] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/moktar/.cabal/bin"
libdir     = "/home/moktar/.cabal/lib/i386-linux-ghc-7.10.3/Reserv-0.1.0.0-GbbvlLC5RmABms7t6gLWOV"
datadir    = "/home/moktar/.cabal/share/i386-linux-ghc-7.10.3/Reserv-0.1.0.0"
libexecdir = "/home/moktar/.cabal/libexec"
sysconfdir = "/home/moktar/.cabal/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "Reserv_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "Reserv_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "Reserv_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "Reserv_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "Reserv_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
