{-# LANGUAGE OverloadedLists, TypeFamilies #-}
module Core where

import GHC.Exts
import qualified Prelude as P
import Prelude (Show(..), Int, Num(..), fromIntegral)

data Bool = False | True deriving (Show)

not :: Bool -> Bool
not = \x ->
    case x of
        False -> True
        True  -> False

notnot :: Bool -> Bool
notnot = \x -> not (not x)

id :: Bool -> Bool
id = \x -> x

and :: Bool -> Bool -> Bool
and = \x y ->
    case x of
        False -> False
        True -> y

(&&), (||) :: Bool -> Bool -> Bool
(&&) = and

(||) = \x y ->
  case x of
    False -> y
    True -> True

data Nat = Zero | Succ Nat

addNat :: Nat -> Nat -> Nat
addNat = \x y ->
  case x of
    Zero -> y
    (Succ a) -> Succ (addNat a y)

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


data Stream = Neck Nat Stream deriving (Show)

--Neck :: Nat -> Stream -> Stream

zeroes :: Stream
zeroes = Neck Zero zeroes

head :: Stream -> Nat
head = \s ->
  case s of
    (Neck x xs) -> x

tail :: Stream -> Stream
tail = \s ->
  case s of
    (Neck x xs) -> xs

nats :: Stream
nats = natsFrom Zero

natsFrom :: Nat -> Stream
natsFrom = \n -> Neck n (natsFrom (Succ n))

intToNat = \x ->
  case x of
    0 -> Zero
    n -> Succ (intToNat (n-1))

natToInt :: Nat -> Int
natToInt = \x ->
  case x of
    Zero -> 0
    (Succ a) -> (natToInt a) + 1

instance Num Nat where
  (+) = addNat
  fromInteger = \x ->
    intToNat (fromIntegral x)

instance Show Nat where
  show = \x -> show (natToInt x)


map :: (Nat -> Nat) -> (Stream -> Stream)
map = \f s ->
  case s of
    (Neck x xs) -> Neck (f x) (map f xs)

ones :: Stream
ones = map Succ zeroes

filter :: (Nat -> Bool) -> (Stream -> Stream)
filter = \p s ->
  case s of
    (Neck x xs) ->
      case p x of
        False -> filter p xs
        True -> Neck x (filter p xs)

data List = Nil | Cons Nat List

--List functions
mapList :: (Nat -> Nat) -> (List -> List)
mapList = \f s ->
          case s of
            Nil -> Nil
            (Cons x xl) -> Cons (f x) (mapList f xl)

filterList :: (Nat -> Bool) -> (List -> List)
filterList = \p s ->
  case s of
    Nil -> Nil
    (Cons x xl) ->
      case p x of
        False -> filterList p xl
        True -> Cons x (filterList p xl)

zeroesList :: List
zeroesList = Cons Zero zeroesList

instance Show List where
  show = \x -> show (toList x)

instance IsList List where
  type Item List = Nat
  fromList = P.foldr Cons Nil
  toList = \x ->
    case x of
      Nil -> []
      Cons n xs -> n : toList xs

(++) :: List -> List -> List
(++) = \x y ->
  case x of
    Nil -> y
    (Cons x xl) -> Cons x (xl ++ y)

multNat :: Nat -> Nat -> Nat
multNat = \x y ->
  case x of
    Zero -> Zero
    (Succ a) -> y + (multNat a y)