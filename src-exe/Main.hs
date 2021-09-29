module Main where

import qualified Pristinum.Test (someFunc)

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  Pristinum.Test.someFunc
