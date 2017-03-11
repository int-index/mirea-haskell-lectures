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

-- sum :: [Nat] -> Nat
sum' :: Num a => [a] -> a
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


class Mappable container where
  cmap :: (a -> b) -> container a -> container b

instance Mappable [] where
  cmap f []       = []
  cmap f (x : xs) = f x : cmap f xs

instance Mappable Maybe where
  cmap f Nothing  = Nothing
  cmap f (Just a) = Just (f a)

instance Mappable Stream where
  cmap f (x :> xs) = f x :> cmap f xs

instance Mappable Tree where
  cmap f (Leaf a)           = Leaf (f a)
  cmap f (Branch a lst rst) =
    Branch (f a) (cmap f lst) (cmap f rst)
