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