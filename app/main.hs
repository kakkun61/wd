import Distribution.At    (at)
import System.Environment (getArgs)
import System.IO          (Handle, hPutStrLn, stderr)

main :: IO ()
main = do
  args <- getArgs
  case args of
    dir:cmd:args -> at dir cmd args
    _ -> do
      hPutStrLn stderr "Error: At least 2 arguments are necessary."
      printUsage stderr

printUsage :: Handle -> IO ()
printUsage h = hPutStrLn h "Usage: at DIR CMD [ARGS]"
