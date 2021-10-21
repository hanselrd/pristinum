module Main where

-- import Data.String.Conversions
-- import qualified Data.Text as T
import qualified Data.Text.IO as T
-- import LLVM.Pretty
import Options.Applicative
-- import Prettyprinter
-- import Prettyprinter.Render.Text
import Pristinum
import Text.Pretty.Simple

data Action
  = Lex
  | Ast
  | Sast
  | LLVM
  | Compile FilePath
  | Run
  deriving (Show, Eq)

data Options = Options
  { action :: Action,
    inFile :: FilePath,
    verbose :: Bool
  }
  deriving (Show, Eq)

actionP :: Parser Action
actionP =
  flag' Lex (long "lex" <> short 'l' <> help "Print the Lexeme stream")
    <|> flag' Ast (long "ast" <> short 'a' <> help "Print the AST")
    <|> flag'
      Sast
      ( long "sast" <> short 's' <> help "Print the semantically checked AST"
      )
    <|> flag'
      LLVM
      (long "llvm" <> short 'l' <> help "Print the generated LLVM bytecode")
    <|> flag'
      Compile
      (long "compile" <> short 'c' <> help "Compile to an executable")
    <*> strOption
      (long "out" <> short 'o' <> value "a.out" <> metavar "OUTPUT_FILE")
    <|> pure Run

optionsP :: Parser Options
optionsP =
  Options
    <$> actionP
    <*> strArgument (help "Input file" <> metavar "INPUT_FILE")
    <*> switch (long "verbose" <> short 'v' <> help "Verbosity level")

runOpts :: Options -> IO ()
runOpts (Options act inF _) = do
  input <- T.readFile inF
  let ts = myLexer input
  case act of
    Lex -> pPrint ts
    Ast -> case pProgram ts of
      Left err -> pPrint err
      Right ast -> pPrint ast
    Sast -> pPrint ()
    LLVM -> pPrint ()
    Compile s -> pPrint s
    Run -> pPrint ()

main :: IO ()
main = runOpts =<< execParser opts
  where
    opts =
      info
        (optionsP <**> helper)
        ( fullDesc
            <> progDesc
              "Run the Pristinum compiler on the given file. \
              \Passing no flags will compile the file, execute it and print the output."
            <> header "Pristinum Compiler"
            <> footer "(C) Pristinum Compiler Development Group"
        )
