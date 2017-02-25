{-# LANGUAGE TemplateHaskell, UnicodeSyntax #-}

module Challenge where

import Prelude()
import Magic

data Nat = Succ Nat | Zero
magicNat ''Nat

data Bool = False | True
  deriving (Show)

data Maybe α = Nothing | Just α
  deriving (Show)

{- Helpers -}

(.) :: (β -> γ) -> (α -> β) -> (α -> γ)
(.) f g x = f (g x)

(+), (*) :: Nat -> Nat -> Nat

x + y =
  case x of
    Zero   -> y
    Succ a -> Succ (a + y)

x * y =
  case x of
    Zero   -> Zero
    Succ a -> y + (a * y)

flip :: (α -> β -> γ) ->
        (β -> α -> γ)
flip = \f x y -> f y x

even, odd :: Nat -> Bool

even x =
  case x of
    Zero   -> True
    Succ a -> odd a

odd x =
  case x of
    Zero   -> False
    Succ a -> even a

id :: α -> α
id = \x -> x

const :: alpha -> beta -> alpha
const = \a b -> a

(&&), (||) :: Bool -> Bool -> Bool

x && y =
  case x of
    False -> False
    True  -> y

x || y =
  case x of
    False -> y
    True  -> True

foldr :: (α -> β -> β) ->
          β ->
          [α] ->
          β
foldr f acc list =
  case list of
    []      -> acc
    x : xs  -> f x (foldr f acc xs)

map :: (α -> β) -> [α] -> [β]
map f =
  foldr ((:).f) []

(++) :: [α] -> [α] -> [α]
(++) =
  flip (foldr (:))

concat :: [[α]] -> [α]
concat = foldr (++) []

filter :: (α -> Bool) -> [α] -> [α]
filter f = foldr (\x -> ifThenElse (f x) (x:) id) []

ifThenElse :: Bool -> α -> α -> α
ifThenElse cond thenCase elseCase =
  case cond of
    True  -> thenCase
    False -> elseCase

length :: [α] -> Nat
length = foldr (const Succ) 0

null :: [α] -> Bool
null = foldr (const (&& False)) True
