module Pristinum.CLI
  ( parseCli,
  )
where

import Options.Applicative

data CLI = CLI
  { file :: String,
    verbose :: Bool
  }
  deriving (Show, Eq)

cli :: Parser CLI
cli =
  CLI
    <$> strOption
      (long "file" <> short 'f' <> metavar "FILE" <> help "File to parse")
    <*> switch (long "verbose" <> short 'v' <> help "Enable verbose output")

parseCli :: IO CLI
parseCli = execParser opts
  where
    opts =
      info
        (cli <**> helper)
        ( fullDesc <> progDesc "Print a greeting for TARGET"
            <> header
              "hello - a test for optparse-applicative"
        )
