module CoreP where

-- types
--
-- Char
-- [a]
-- Integer
-- Int
-- Double
-- Rational
-- Bool

-- classes
-- Eq a
--  (==), (/=)
-- Ord a
--  compare, (>=), (<=), (>), (<)
-- Num a
--  (+), (-), (*), negate, abs, fromInteger
-- Fractional a
--  (/), fromRational
-- Real a
--  toRational
-- Show a
--  show
-- Functor a
--  fmap
-- Foldable a
--  foldr

data Nat = Succ Nat | Zero

sum' :: Foldable f => Num a => f a -> a
sum' = foldr (+) 0

data Tree a = Leaf a | Branch a (Tree a) (Tree a)
  deriving (Show, Eq, Ord)

data Stream a = a :> Stream a
  deriving (Show, Eq, Ord)

{-

          Branch 5
        /         \
      Leaf 7    Branch 3
              /           \
            Branch 0      Leaf 8
          /         \
        Leaf 3    Leaf 8

-}

treeSample :: Tree Integer
treeSample =
  Branch 5
    (Leaf 7)
    (Branch 3
      (Branch 0
        (Leaf 3)
        (Leaf 8))
      (Leaf 8))

instance Functor Stream where
  fmap f (x :> xs) = f x :> fmap f xs

instance Functor Tree where
  fmap f (Leaf a)           = Leaf (f a)
  fmap f (Branch a lst rst) =
    Branch (f a) (fmap f lst) (fmap f rst)

instance Foldable Tree where
  foldr cons nil = foldr cons nil . treeToList

treeToList :: Tree a -> [a]
treeToList (Leaf a) = [a]
treeToList (Branch a lst rst) =
  treeToList lst ++
  [a] ++
  treeToList rst

data Ten = A | B | C | D | E | F | G | H | K | L
  deriving (Show, Eq, Ord)

data Pair a = P
  { pfst :: a
  , psnd :: a
  } deriving (Show)

instance Functor Pair where
  fmap f (P x y) = P (f x) (f y)
