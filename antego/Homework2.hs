{-# LANGUAGE OverloadedLists, TypeFamilies #-}
module Homework2 where

import GHC.Exts
import qualified Prelude as P
import Prelude (Show(..), Int, Num(..), fromIntegral)

data Nat = Zero | Succ Nat
data Bool = False | True deriving (Show)

data ListNat = NilNat | ConsNat Nat ListNat
data ListBool = NilBool | ConsBool Bool ListBool
--data ListX = NilX | ConsX X ListX
--data ListListNat = NilListNat | ConsListNat ListNat ListListNat

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

-- Show Nat as decimals
addNat :: Nat -> Nat -> Nat
addNat = \x y ->
  case x of
    Zero -> y
    (Succ a) -> Succ (addNat a y)

multNat :: Nat -> Nat -> Nat
multNat = \x y ->
  case x of
    Zero -> Zero
    (Succ a) -> y + (multNat a y)

intToNat :: Int -> Nat
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
  (*) = multNat
  fromInteger = \x ->
    intToNat (fromIntegral x)

instance Show Nat where
  show = \x -> show (natToInt x)

-- Print and write Lists in brackets form
instance Show ListNat where
  show = \x -> show (toList x)

instance IsList ListNat where
  type Item ListNat = Nat
  fromList = P.foldr ConsNat NilNat
  toList = \x ->
    case x of
      NilNat -> []
      ConsNat n xs -> n : toList xs

foldNat :: (Nat -> Nat -> Nat) -> Nat -> ListNat -> Nat
foldNat = \f x list ->
  case list of
    NilNat -> x
    (ConsNat n nl) -> foldNat f (f x n) nl

sum, product :: ListNat -> Nat
sum     = foldNat (+) Zero
product = foldNat (*) (Succ Zero)

foldBool :: (Bool -> Bool -> Bool) -> Bool -> ListBool -> Bool
foldBool = \f x list ->
  case list of
    NilBool -> x
    (ConsBool b bl) -> foldBool f (f x b) bl

listAnd, listOr :: ListBool -> Bool
listAnd = foldBool (&&) True
listOr  = foldBool (||) False