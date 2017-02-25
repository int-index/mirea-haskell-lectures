{-# LANGUAGE TemplateHaskell #-}
module Challenge where

import Prelude()
import Magic

data Nat = Succ Nat | Zero

magicNat ''Nat

data Bool = True | False
  deriving Show

(+), (*) :: Nat -> Nat -> Nat
(+) = \x y ->
    case x of
      Zero -> y
      (Succ x') -> Succ (x' + y)

(*) = \x y ->
    case x of
      Zero -> Zero
      (Succ x') -> y + (x' * y)

(.) :: (b -> c) -> (a -> b) -> a -> c
(.) f g = \x -> f (g x)
infixr .

foldr :: (element -> accum -> accum) ->
 accum -> [element] -> accum
foldr func element list =
  case list of
    [] -> element
    (element' : list') -> func element' (foldr func element list')

foldl :: (accum -> element -> accum) -> accum -> [element] -> accum
foldl func element list =
  case list of
    [] -> element
    (element' : list') -> foldl func (func element element') list'

map :: (before -> after) -> [before] -> [after]
map func list =
  case list of
    [] -> []
    (element' : list') -> (func element') : (map func list')
