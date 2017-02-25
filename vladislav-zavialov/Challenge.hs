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

-- data List element = Nil | Cons element (List element)
-- data [element] = []  | (:)  element [element]

-- [1,2,3] = 1 : 2 : 3 : []

foldr ::
  (element -> accum -> accum) ->
  accum ->
  [element] ->
  accum
foldr cons nil xs =
  case xs of
    []      -> nil
    x : xs' -> cons x (foldr cons nil xs')



