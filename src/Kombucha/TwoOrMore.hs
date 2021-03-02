module Kombucha.TwoOrMore where

data TwoOrMore a = TwoOrMore a a [a]
  deriving (Eq, Show)
