{-# LANGUAGE TemplateHaskell #-}
module Challenge where

import Prelude()
import Magic

data Bool = -- Bool- класс (тип)
 False | True -- конструктор
 deriving (Show)

data Nat = Zero | Succ Nat
magicNat ''Nat

data List alpha = Nil | Cons alpha (List alpha)
-- data [alpha] = [] | (:) alpha [alpha]

(+) :: Nat -> Nat -> Nat
(+) = \x y ->
 case x of 
  Zero -> y
  (Succ a) -> Succ (a + y)

(*) :: Nat -> Nat -> Nat
(*) = \x y ->
 case x of
  Zero -> x
  (Succ a) -> y + (a * y)

foldr :: (element -> accum -> accum) -> accum -> [element] -> accum

foldr f x xs =
	case xs of 
		[] -> x
		s : xs' -> f s (foldr f x xs')

map :: (before -> after) -> ([before] -> [after])
map f xs =
	case xs of 
		[] -> []
		x:xs -> (f x) : (map f xs)

map' f xs = foldr ((:) . f) [] xs

(.) :: (beta -> gamma) -> (alpha -> beta) -> (alpha -> gamma)
(.) f g x = f (g x)


(++) :: [element] -> [element] -> [element]
(++) xs ys =
 case xs of
  [] -> ys
  x : xs -> x : (xs ++ ys)

(+++) = flip (foldr (:))

flip :: (alpha -> beta -> gamma) -> (beta -> alpha -> gamma)
flip = \f x y -> f y x