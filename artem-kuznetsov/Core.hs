{-# LANGUAGE OverloadedLists, TypeFamilies #-}
module Core where

import GHC.Exts
import qualified Prelude as P
import Prelude (Show(..), Int, Num(..), fromIntegral)

data Nat = Zero | Succ Nat

-- (intToNat 4) = Succ (Succ (Succ (Succ Zero)))

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
  (*) = mulNat
  fromInteger = \x ->
    intToNat (fromIntegral x)

instance Show Nat where
  show = \x -> show (natToInt x)

-- > head (tail (natsFrom 5))
-- 6

instance Show ListNat where
  show = \x -> show (toList x)

instance IsList ListNat where
  type Item ListNat = Nat
  fromList = P.foldr ConsNat NilNat
  toList = \x ->
    case x of
      NilNat -> []
      (ConsNat n xs) -> n : toList xs

-- :set -XOverloadedLists
-- [2, 0] :: List
-- Cons (Succ (Succ Zero)) (Cons Zero Nil)


-- Some more magic code, please ignore
($) :: (a -> b) -> a -> b
($) f x = f x
(.) :: (b -> c) -> (a -> b) -> a -> c
(.) f g = \x -> f (g x)


data Bool = True | False
  deriving Show

not :: Bool -> Bool
not = \x ->
  case x of
    True -> False
    False -> True

(&&) :: Bool -> Bool -> Bool
(&&) = \x y ->
  case x of
    False -> False
    True -> y

(||) :: Bool -> Bool -> Bool
(||) = \x y ->
  case x of
    False -> y
    True -> True

addNat :: Nat -> Nat -> Nat
addNat = \x y ->
  case x of
    Zero -> y
    (Succ x') -> Succ (x' + y)

mulNat :: Nat -> Nat -> Nat
mulNat = \x y ->
  case x of
    Zero -> Zero
    (Succ x') -> y + (x' * y)

odd :: Nat -> Bool
odd = \x ->
  case x of
    Zero -> True
    (Succ a) -> even a

even :: Nat -> Bool
even = \x -> not (odd x)

(==), (/=), (>), (<), (<=), (>=) :: Nat -> Nat -> Bool
(>) = \x y ->
  case x of
    Zero -> False
    Succ x' ->
      case y of
        Zero -> True
        Succ y' -> x' > y'

(<=) = \x y -> not (x > y)
(<)  = \x y -> y > x
(>=) = \x y -> not (x < y)
(==) = \x y -> (x >= y) && (x <= y)
(/=) = \x y -> not (x == y)

data Stream = Neck Nat Stream
  deriving Show

head :: Stream -> Nat
head = \xs ->
  case xs of
    (Neck x _) -> x

tail :: Stream -> Stream
tail = \xs ->
  case xs of
    (Neck _ xs') -> xs'

repeat :: Nat -> Stream
repeat = \x -> Neck x (repeat x )

iterate :: (Nat -> Nat) -> Nat -> Stream
iterate = \f x -> Neck x (iterate f (f x))

(!!) :: Nat -> Stream -> Nat
(!!) = \x s ->
  case x of
    Zero -> head s
    (Succ n) -> n !! tail s

mapStream :: (Nat -> Nat) -> Stream -> Stream
mapStream = \f s ->
  case s of
    (Neck x s') -> Neck (f x) (mapStream f s')

filterStream :: (Nat -> Bool) -> Stream -> Stream
filterStream = \p s ->
  case s of
    (Neck x s') ->
      case p x of
        True -> Neck x (filterStream p s')
        False -> filterStream p s'

data ListNat = NilNat | ConsNat Nat ListNat
data ListBool = NilBool | ConsBool Bool ListBool
  deriving Show
data ListListNat = NilListNat | ConsListNat ListNat ListListNat
  deriving Show

-- Test list
l, l2 :: ListNat
l = fromList [1,2,3,4,5,6,7,8,9,10]
l2 = fromList [1,1,2,2,3,3,4,4,5,5,6,7,8,9,10]

streamToList :: Stream -> ListNat
streamToList = \s ->
  case s of
    (Neck x s) -> ConsNat x (streamToList s)
 
foldNat :: (Nat -> Nat -> Nat) -> Nat -> ListNat -> Nat
foldNat = \f x xs ->
  case xs of
    NilNat -> x
    ConsNat x' xs' -> f x' (foldNat f x xs')

foldBool :: (Bool -> Bool -> Bool) -> Bool -> ListBool -> Bool
foldBool = \f x xs ->
  case xs of
    NilBool -> x
    ConsBool x' xs' -> f x' (foldBool f x xs')

foldListNat :: (ListNat -> ListNat -> ListNat) -> ListNat -> ListListNat -> ListNat
foldListNat = \f x xs ->
  case xs of
    NilListNat -> x
    ConsListNat x' xs' -> f x' (foldListNat f x xs')

appendNat :: ListNat -> ListNat -> ListNat
appendNat = \xs ys ->
  case xs of
    NilNat -> ys
    ConsNat x xs -> ConsNat x (appendNat xs ys)

mapNatBool :: (Nat -> Bool) -> ListNat -> ListBool
mapNatBool = \p xs ->
  case xs of
    NilNat -> NilBool
    ConsNat x xs' -> ConsBool (p x) (mapNatBool p xs')

sum, product :: ListNat -> Nat
sum = foldNat (+) Zero
product = foldNat (*) (Succ Zero)

and, or :: ListBool -> Bool
and = foldBool (&&) True
or = foldBool (||) False

all, any :: (Nat -> Bool) -> ListNat -> Bool
all = \p xs -> and (mapNatBool p xs)
any = \p xs -> or  (mapNatBool p xs)

concat :: ListListNat -> ListNat
concat = foldListNat (appendNat) NilNat

reverse :: ListNat -> ListNat
reverse = \xs ->
  case xs of
    NilNat -> NilNat
    ConsNat x xs' -> appendNat (reverse xs') (ConsNat x NilNat)


-- Alternative implementation of reverse
reverse_ :: ListNat -> ListNat -> ListNat
reverse_ = \acc xs ->
  case xs of
    NilNat -> acc
    ConsNat x xs' -> reverse_ (ConsNat x acc) xs'

reverse' :: ListNat -> ListNat
reverse' = reverse_ NilNat

prependStream :: ListNat -> Stream -> Stream
prependStream = \xs s ->
  case xs of
    NilNat -> s
    ConsNat x xs' -> Neck x (prependStream xs' s)

cycle :: ListNat -> Stream
cycle = \xs -> prependStream xs (cycle xs)

takeListNat :: Nat -> ListNat -> ListNat
takeListNat = \n xs ->
  case n of
    Zero -> NilNat
    (Succ n') ->
      case xs of
        NilNat -> NilNat
        (ConsNat x xs') -> ConsNat x (takeListNat n' xs')

takeStream :: Nat -> Stream -> ListNat
takeStream = \n s -> takeListNat n (streamToList s)

replicate :: Nat -> Nat -> ListNat
replicate = \n x -> takeStream n (repeat x)

filterNat :: (Nat -> Bool) -> ListNat -> ListNat
filterNat = \p xs ->
  case xs of
    NilNat -> NilNat
    ConsNat x xs' ->
      case p x of
        False -> filterNat p xs'
        True -> ConsNat x (filterNat p xs')

takeWhileListNat :: (Nat -> Bool) -> ListNat -> ListNat
takeWhileListNat = \p xs ->
  case xs of
    NilNat -> NilNat
    ConsNat x xs' ->
      case p x of
        False -> NilNat
        True -> ConsNat x (takeWhileListNat p xs')

takeWhileStream :: (Nat -> Bool) -> Stream -> ListNat
takeWhileStream = \p s -> takeWhileListNat p (streamToList s)

dropWhileListNat :: (Nat -> Bool) -> ListNat -> ListNat
dropWhileListNat = \p xs ->
  case xs of
    NilNat -> NilNat
    ConsNat x xs' ->
      case p x of
        False -> ConsNat x xs'
        True -> dropWhileListNat p xs'

dropWhileStream :: (Nat -> Bool) -> Stream -> Stream
dropWhileStream = \p xs ->
  case xs of
    Neck x xs' ->
      case p x of
        False -> Neck x xs'
        True -> dropWhileStream p xs'

null :: ListNat -> Bool
null = \xs ->
  case xs of
    NilNat -> True
    _ -> False

elem :: Nat -> ListNat -> Bool
elem = \x xs -> or (mapNatBool (==x) xs)


group :: ListNat -> ListListNat
group = \xs ->
  case xs of
    NilNat -> NilListNat
    ConsNat x xs' -> ConsListNat (takeWhileListNat (==x) (ConsNat x xs')) (group (dropWhileListNat (==x) xs'))

nub :: ListNat -> ListNat
nub = \xs ->
  case xs of
    NilNat -> NilNat
    ConsNat x xs' -> ConsNat x (nub (filterNat (/=x) xs'))

-- Alternative nub implementation
nub_ :: ListNat -> ListNat -> ListNat
nub_ = \acc xs ->
  case xs of
    NilNat -> reverse acc
    ConsNat x xs' ->
      case elem x acc of
        True -> nub_ acc xs'
        False -> nub_ (ConsNat x acc) xs'

nub' :: ListNat -> ListNat
nub' = nub_ NilNat

data PairListNat = PLN ListNat ListNat
data PairStream = PS Stream Stream

partitionNat :: (Nat -> Bool) -> ListNat -> PairListNat
partitionNat = \f xs -> PLN (filterNat f xs) (filterNat (\x -> not (f x)) xs)
partitionStream :: (Nat -> Bool) -> Stream -> PairStream
partitionStream = \f xs -> PS (filterStream f xs) (filterStream (\x -> not (f x)) xs) 

data PairNat = PN Nat Nat
  deriving Show
data ListPairNat = NilPairNat | ConsPairNat PairNat ListPairNat
  deriving Show

zip :: ListNat -> ListNat -> ListPairNat
zip = \xs ys ->
  case xs of
    NilNat -> NilPairNat
    ConsNat x xs' ->
      case ys of
        NilNat -> NilPairNat
        ConsNat y ys' -> ConsPairNat (PN x y) (zip xs' ys')

length :: ListNat -> Nat
length = \xs ->
  case xs of
    NilNat -> Zero
    ConsNat _ xs' -> Succ (length xs')
