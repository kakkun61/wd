import Lib (hello)

main :: IO ()
main = do
  putStrLn $ hello "Pepe"
