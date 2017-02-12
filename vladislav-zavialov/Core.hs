{-# LANGUAGE OverloadedLists, TypeFamilies #-}

module Core where

import qualified Prelude as P
import GHC.Exts

import Prelude (Show(..), Int, Num(..), fromIntegral)

intToNat = \x ->
  case x of
    0 -> Zero
    n -> Succ (intToNat (n-1))

natToInt = \x ->
  case x of
    Zero -> 0
    (Succ a) -> (natToInt a) + 1

instance Num Nat where
  (+) = addNat
  (*) = multNat
  fromInteger = \x ->
    intToNat (fromIntegral x)

instance Show Nat where
  show = \x -> show (natToInt x)

data Bool =
  False | True
  deriving (Show)

not :: Bool -> Bool
not = \x ->
  case x of
    False -> True
    True  -> False

notnot :: Bool -> Bool
notnot = \x -> not (not x)

id :: Bool -> Bool
id = \x -> x

(&&), (||) :: Bool -> Bool -> Bool

(&&) = \x y ->
  case x of
    False -> False
    True  -> y

(||) = \x y ->
  case x of
    False -> y
    True  -> True

data Void

data Unit = U

data Color = Red | Green | Blue

data Suit = Spades | Hearts | Diamonds | Clubs

data Nat = Zero | Succ Nat

addNat :: Nat -> Nat -> Nat
addNat = \x y ->
  case x of
    Zero     -> y
    (Succ a) -> Succ (addNat a y)

multNat :: Nat -> Nat -> Nat
multNat = \x y ->
  case x of
    Zero     -> Zero
    (Succ a) -> (multNat a y) + y

even :: Nat -> Bool
even = \x ->
  case x of
    Zero     -> True
    (Succ a) -> odd a

odd :: Nat -> Bool
odd = \x ->
  case x of
    Zero     -> False
    (Succ a) -> even a

data Age = Years Nat

data Point = XY Nat Nat

data Pixel = RGB Nat Nat Nat

data Shape = Rect Nat Nat Nat Nat

data Stream = Neck Nat Stream

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
natsFrom = \n ->
  Neck n (natsFrom (Succ n))

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
        False ->         filter p xs
        True  -> Neck x (filter p xs)

data ListNat = NilNat | ConsNat Nat ListNat

data ListBool = NilBool | ConsBool Bool ListBool
  deriving (Show)

appendNat :: ListNat -> ListNat -> ListNat
appendNat = \xs ys ->
  case xs of
    NilNat        -> ys
    ConsNat n xs' -> ConsNat n (appendNat xs' ys)

foldNat :: (Nat -> Nat -> Nat) -> Nat -> ListNat -> Nat
foldNat = \cons nil xs ->
  case xs of
    NilNat        -> nil
    ConsNat n xs' -> cons n (foldNat cons nil xs')

foldBool :: (Bool -> Bool -> Bool) -> Bool -> ListBool -> Bool
foldBool = \cons nil xs ->
  case xs of
    NilBool        -> nil
    ConsBool n xs' -> cons n (foldBool cons nil xs')

sum, product :: ListNat -> Nat
sum     = foldNat (+) Zero
product = foldNat (*) (Succ Zero)

and, or :: ListBool -> Bool
and = foldBool (&&) True
or  = foldBool (||) False

reverse :: ListNat -> ListNat
reverse = \xs ->
  case xs of
    NilNat        -> NilNat
    ConsNat n xs' -> appendNat xs' (ConsNat n NilNat)

instance Show ListNat where
  show = \x -> show (toList x)

instance IsList ListNat where
  type Item ListNat = Nat
  fromList = P.foldr ConsNat NilNat
  toList = \x ->
    case x of
      NilNat -> []
      ConsNat n xs -> n : toList xs
