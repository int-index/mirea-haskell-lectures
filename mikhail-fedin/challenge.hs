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
-- map func list = foldr (\element list' -> func element : list') [] list
-- map func = foldr ((:) . func) []

const :: a -> b -> a
const x _ = x

id :: a -> a
id x = x

flip :: (a -> b -> c) -> b -> a -> c
flip f x y = f y x

(++) :: [element] -> [element] -> [element]
--(++) list1 list2 = foldr (:) list2 list1
(++) = flip (foldr (:))

sum :: [Nat] -> Nat
sum = foldr (+) 0

product :: [Nat] -> Nat
product = foldr (*) 1

concat :: [[element]] -> [element]
concat = foldr (++) []

not :: Bool -> Bool
not x =
  case x of
    True -> False
    False -> True

odd, even :: Nat -> Bool
even x =
  case x of
    Zero -> True
    (Succ a) -> odd a
odd x = not (even x)

filter :: (element -> Bool) -> [element] -> [element]
filter func list =
  case list of
    [] -> []
    (element' : list') ->
     case func element' of
      False -> filter func list'
      True ->  element' : (filter func list')
--filter func list = foldr
-- (\element list -> case func element of
--    True -> element : list
--    False -> list)
--  []

ifThenElse :: Bool -> a -> a -> a
ifThenElse cond thenCase elseCase =
 case cond of
  True -> thenCase
  False -> elseCase
--filter func = foldr (\element -> ifThenElse (func element) (element :) id) []

length :: [element] -> Nat
length = foldr (const (Succ)) Zero

null :: [element] -> Bool
--null list =
--  case list of
--    [] -> True
--    _ -> False
null = foldr []
