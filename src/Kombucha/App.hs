module Kombucha.App (main) where

import qualified Data.Map as Map
import Kombucha.Inference
import Kombucha.Verifier
import Prettyprinter
import Prettyprinter.Render.Terminal
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
        Left err -> do
          pHPrint stderr err
          exitFailure
        Right env -> do
          let summary =
                "OK:"
                  <+> pretty (Map.size $ types env)
                  <+> "types declared and"
                  <+> pretty (Map.size $ terms env)
                  <+> "inferences verified."

          render $ pretty env <> line <> line <> annotate (color Green) summary <> line
    _ -> do
      hPutStrLn stderr "Usage: kombucha <filename>"
      exitFailure

render :: Doc AnsiStyle -> IO ()
render = renderIO stdout . layoutPretty (defaultLayoutOptions {layoutPageWidth = Unbounded})
