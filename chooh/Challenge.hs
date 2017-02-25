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
