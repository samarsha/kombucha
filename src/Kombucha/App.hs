module Kombucha.App (main) where

import Kombucha.Verifier
import System.Environment
import System.Exit
import System.IO
import Text.Pretty.Simple

main :: IO ()
main = do
  args <- getArgs
  case args of
    [filename] -> do
      text <- readFile filename
      case verify text of
        Left err -> hPrint stderr err
        Right predicates -> pPrint predicates
    _ -> do
      hPutStrLn stderr "Usage: kombucha <filename>"
      exitFailure
