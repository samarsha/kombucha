{-# LANGUAGE FlexibleInstances #-}

module Kombucha.App (main) where

import Data.Function
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Kombucha.SyntaxTree
import qualified Kombucha.TwoOrMore as TwoOrMore
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
        Left err -> do
          pHPrint stderr err
          exitFailure
        Right predicates -> putStrLn (prettyPrint predicates)
    _ -> do
      hPutStrLn stderr "Usage: kombucha <filename>"
      exitFailure

class PrettyPrintable a where
  prettyPrint :: a -> String

instance PrettyPrintable (Map Name [Predicate]) where
  prettyPrint m = Map.toList m & map prettyPrint & intercalate "\n\n"

instance PrettyPrintable (Name, [Predicate]) where
  prettyPrint (name, pred) = name ++ " is valid when:" ++ (pred & map prettyPrint & map (\s -> "\n" ++ s) & concat)

instance PrettyPrintable Predicate where
  prettyPrint (IsResource type') = prettyPrint type' ++ " is Resource"
  prettyPrint (IsParam type' name) = prettyPrint type' ++ " is " ++ name

instance PrettyPrintable Type where
  prettyPrint (TypeInference (type1 :|- type2)) = prettyPrint type1 ++ " |- " ++ prettyPrint type2
  prettyPrint (TypeResource resource) = prettyPrint resource
  prettyPrint (TypeParam name) = name
  prettyPrint (TypeVariable name) = name

instance PrettyPrintable Resource where
  prettyPrint ResourceUnit = "0"
  prettyPrint (ResourceAtom name types) = name ++ (types & map prettyPrint & map (\s -> " " ++ s) & concat)
  prettyPrint (ResourceTuple types) = "(" ++ (types & TwoOrMore.toList & map prettyPrint & intercalate ", ") ++ ")"