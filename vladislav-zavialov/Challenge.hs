{-# LANGUAGE TemplateHaskell #-}
module Challenge where

import Prelude()
import Magic

data Bool =
  False | True
  deriving (Show)

data Nat = Succ Nat | Zero
magicNat ''Nat

(+), (*) :: Nat -> Nat -> Nat

x + y =
  case x of
    Zero   -> y
    Succ a -> Succ (a + y)

x * y =
  case x of
    Zero   -> Zero
    Succ a -> y + (a * y)

foldr ::
  (element -> accum -> accum) ->
  accum ->
  [element] ->
  accum
foldr cons nil xs =
  case xs of
    []      -> nil
    x : xs' -> cons x (foldr cons nil xs')

f . g = \x -> f (g x)

map :: (before -> after) -> [before] -> [after]
map f = foldr ((:) . f) []

flip f a b = f b a

(++) :: [element] -> [element] -> [element]
(++) = flip (foldr (:))

sum :: [Nat] -> Nat
sum = foldr (+) 0

product :: [Nat] -> Nat
product = foldr (*) 1

concat :: [[element]] -> [element]
concat = foldr (++) []

even, odd :: Nat -> Bool

even x =
  case x of
    Zero   -> True
    Succ a -> odd a

odd x =
  case x of
    Zero   -> False
    Succ a -> even a

ifThenElse :: Bool -> a -> a -> a
ifThenElse cond thenCase elseCase =
  case cond of
    True  -> thenCase
    False -> elseCase

-- ifThenElse with arguments flipped
bool :: a -> a -> Bool -> a
bool onFalse onTrue b =
  case b of
    False -> onFalse
    True  -> onTrue

id :: a -> a
id x = x

-- applicative 2-argument lifting specialised to functions
liftA2 f g h = \x -> f (g x) (h x)

filter :: (element -> Bool) -> [element] -> [element]
filter p = foldr (liftA2 (bool id) (:) p) []

const x = \_ -> x

length = foldr (const Succ) Zero

null :: [element] -> Bool
null = foldr ((const . const) False) True
