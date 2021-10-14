import Pristinum.CLI

main :: IO ()
main = do
  cli <- parseCli
  print cli
