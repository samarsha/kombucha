module Kombucha.TwoOrMore where

data TwoOrMore a = TwoOrMore a a [a]
  deriving (Eq, Show)

instance Functor TwoOrMore where
  fmap f (TwoOrMore x1 x2 xs) = TwoOrMore (f x1) (f x2) $ map f xs

instance Foldable TwoOrMore where
  foldMap f (TwoOrMore x1 x2 xs) = f x1 <> f x2 <> mconcat (map f xs)

instance Traversable TwoOrMore where
  traverse f (TwoOrMore x1 x2 xs) = TwoOrMore <$> f x1 <*> f x2 <*> traverse f xs

toList :: TwoOrMore a -> [a]
toList (TwoOrMore x1 x2 xs) = x1 : x2 : xs

fromList :: [a] -> TwoOrMore a
fromList (x1 : x2 : xs) = TwoOrMore x1 x2 xs
fromList _ = error "List has fewer than two items."
