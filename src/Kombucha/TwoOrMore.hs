module Kombucha.TwoOrMore where

data TwoOrMore a = TwoOrMore a a [a]
  deriving (Eq, Show)

toList :: TwoOrMore a -> [a]
toList (TwoOrMore x1 x2 xs) = x1 : x2 : xs
