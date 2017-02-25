{-# LANGUAGE TemplateHaskell #-}
module Challenge where

import Prelude()
import Magic

data Bool = False | True deriving (Show)

data Nat = Zero | Succ Nat

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

foldr :: (element -> accum -> accum) ->
  accum ->
  [element] ->
  accum

foldr = \f accum list ->
  case list of
    [] -> accum
    a : tail -> f a (foldr f accum tail)

map :: ( before -> after ) -> ([before] -> [after])
map = \f listFrom ->
  case listFrom of
    [] -> []
    a : tail -> (f a) : (map f tail)

(++) :: [element] -> [element] -> [element]
(++) = \firstList secondList -> foldr (:) secondList firstList

concat :: [[element]] -> [element]
concat = foldr (++) []

even :: Nat -> Bool
even = \x ->
  case x of
    Zero -> True
    (Succ a) -> odd a

odd :: Nat -> Bool
odd = \x ->
  case x of
    Zero -> False
    (Succ a) -> even a

ifThenElse :: Bool -> a -> a -> a
ifThenElse cond thenCase elseCase =
  case cond of
    True -> thenCase
    False -> elseCase

id :: alpha -> alpha
id = \x -> x

filter :: (element -> Bool) -> [element] -> [element]
filter = \f -> foldr (\first -> ifThenElse (f first) (first :) id) []

const :: alpha -> beta -> alpha
const = \a b -> a

length :: [element] -> Nat
length = \list -> foldr (\a b -> b+1) 0 list