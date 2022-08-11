module Distribution.At
  ( at
  ) where

import System.Directory (withCurrentDirectory)
import System.Process   (callProcess)

at
  :: FilePath -- ^ Directory.
  -> FilePath -- ^ Command.
  -> [String] -- ^ Arguments.
  -> IO ()
at dir cmd args = withCurrentDirectory dir $ callProcess cmd args
