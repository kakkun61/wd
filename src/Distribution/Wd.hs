module Distribution.Wd
  ( wd
  ) where

import System.Directory (withCurrentDirectory)
import System.Process   (callProcess)

wd
  :: FilePath -- ^ Directory.
  -> FilePath -- ^ Command.
  -> [String] -- ^ Arguments.
  -> IO ()
wd dir cmd args = withCurrentDirectory dir $ callProcess cmd args
