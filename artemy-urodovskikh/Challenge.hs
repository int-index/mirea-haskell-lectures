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


(++), (+++) :: [element] -> [element] -> [element]
(++) xs ys =
 case xs of
  [] -> ys
  x : xs -> x : (xs ++ ys)

(+++) = flip (foldr (:))

flip :: (alpha -> beta -> gamma) -> (beta -> alpha -> gamma)
flip = \f x y -> f y x

concat :: [[element]] -> [element]
{-concat xs =
	case xs of 
		[] -> []
		x:xs' -> case x of
			x:xs'' -> concat xs'-}

concat = foldr (++) []

filter :: (element -> Bool) -> [element] -> [element]
filter p xs =
	case xs of
		[] -> []
		x:xs -> case p x of
			True -> x : (filter p xs)
			False -> filter p xs

-- filter' p = foldr (\x xs -> ifThenElse (p x) (x:) xs) []
filter' = \p -> foldr (\x -> ifThenElse (p x) (x:) id) []

id :: alpha -> alpha
id = \x -> x

odd :: Nat -> Bool
odd = \x ->
 case x of
  Zero -> False
  (Succ a) -> even a

even :: Nat -> Bool
even = \x ->
 case x of 
  Zero -> True
  (Succ a) -> odd a

ifThenElse :: Bool -> a -> a -> a
ifThenElse cond thenCase elseCase = 
	case cond of 
		True -> thenCase
		False -> elseCase

const :: alpha -> beta -> alpha
const = \a b -> a

length :: [element] -> Nat
length = foldr ((+) . (const 1)) 0 