module Main where

import qualified Pristinum.ParserTest
import Test.Tasty

tests = testGroup "Unit tests" [Pristinum.ParserTest.tests]

main = defaultMain tests
