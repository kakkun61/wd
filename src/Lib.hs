module Lib
  ( hello
  ) where

-- | Greeting message.
--
-- >>> hello "Pepe"
-- "Hello, Pepe!"
hello :: String -> String
hello name = "Hello, " ++ name ++ "!"
