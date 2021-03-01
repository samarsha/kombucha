module Kombucha.TwoOrMore where

data TwoOrMore a = (a, a) ::| [a]
  deriving (Eq, Show)
