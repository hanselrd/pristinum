module Main where

import Data.String.Conversions
import qualified Data.Text.IO as T
import Pristinum
import System.FilePath
import Test.Tasty
import Test.Tasty.Golden
import Text.Pretty.Simple

digests :: IO TestTree
digests = do
  files <- findByExtension [".pst"] "test/digests"
  return $
    testGroup
      "Digest tests"
      [ testGroup
          (takeBaseName file)
          [ goldenVsString
              "lexing"
              (replaceExtension file ".lex.golden")
              ( cs <$> do
                  input <- T.readFile file
                  return (pShowNoColor (myLexer input))
              ),
            goldenVsString
              "parsing"
              (replaceExtension file ".ast.golden")
              ( cs <$> do
                  input <- T.readFile file
                  return
                    ( case pProgram (myLexer input) of
                        Left err -> pShowNoColor err
                        Right ast -> pShowNoColor ast
                    )
              )
          ]
        | file <- files
      ]

main :: IO ()
main = defaultMain =<< digests
