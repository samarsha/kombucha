module Kombucha.App (main) where

import qualified Data.Map as Map
import Kombucha.Inference
import Kombucha.Pretty
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

          renderIO stdout $ reAnnotateS colorize (layout $ prettySyntax env <> line <> line)
          renderIO stdout $ layout (annotate (color Green) summary <> line)
    _ -> do
      hPutStrLn stderr "Usage: kombucha <filename>"
      exitFailure

layout :: Doc ann -> SimpleDocStream ann
layout = layoutPretty $ defaultLayoutOptions {layoutPageWidth = Unbounded}

colorize :: Syntax -> AnsiStyle
colorize SyntaxClaim = bold <> color White
colorize SyntaxKeyword = color Blue
colorize SyntaxOperator = color Red
colorize SyntaxType = color Yellow
