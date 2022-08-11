import System.Directory   (withCurrentDirectory)
import System.Environment (getArgs)
import System.IO          (Handle, hPutStrLn, stderr)
import System.Process     (callProcess)

main :: IO ()
main = do
  args <- getArgs
  case args of
    dir:cmd:args -> withCurrentDirectory dir $ callProcess cmd args
    _ -> do
      hPutStrLn stderr "Error: At least 2 arguments are necessary."
      printUsage stderr

printUsage :: Handle -> IO ()
printUsage h = hPutStrLn h "Usage: wd DIR CMD [ARGS]"
