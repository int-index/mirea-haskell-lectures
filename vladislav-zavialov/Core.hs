{-# LANGUAGE OverloadedLists, TemplateHaskell, TypeFamilies, NoImplicitPrelude #-}

module Core where

import Magic

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

data Nat = Succ Nat | Zero

magicNat ''Nat

(+), (*) :: Nat -> Nat -> Nat

(+) = \x y ->
  case x of
    Zero     -> y
    (Succ a) -> Succ (a + y)

(*) = \x y ->
  case x of
    Zero     -> Zero
    (Succ a) -> (a * y) + y

even, odd :: Nat -> Bool

even = \x ->
  case x of
    Zero     -> True
    (Succ a) -> odd a

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

mapStream :: (Nat -> Nat) -> (Stream -> Stream)
mapStream = \f s ->
  case s of
    (Neck x xs) -> Neck (f x) (mapStream f xs)

ones :: Stream
ones = mapStream Succ zeroes

filterStream :: (Nat -> Bool) -> (Stream -> Stream)
filterStream = \p s ->
  case s of
    (Neck x xs) ->
      case p x of
        False ->         filterStream p xs
        True  -> Neck x (filterStream p xs)

data ListNat = NilNat | ConsNat Nat ListNat
magicList ''ListNat

data ListBool = NilBool | ConsBool Bool ListBool
magicList ''ListBool

data ListListNat = NilListNat | ConsListNat ListNat ListListNat
magicList ''ListListNat

mapListNat :: (Nat -> Nat) -> (ListNat -> ListNat)
mapListNat = \f s ->
  case s of
    NilNat         -> NilNat
    (ConsNat x xs) -> ConsNat (f x) (mapListNat f xs)

filterListNat :: (Nat -> Bool) -> (ListNat -> ListNat)
filterListNat = \p s ->
  case s of
    NilNat         -> NilNat
    (ConsNat x xs) ->
      case p x of
        False ->            filterListNat p xs
        True  -> ConsNat x (filterListNat p xs)

(++) :: ListNat -> ListNat -> ListNat
(++) = \xs ys ->
  case xs of
    NilNat        -> ys
    ConsNat n xs' -> ConsNat n (xs' ++ ys)

-- Homework

-- task 1: foldNat, foldBool

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

-- task 2: mapNatBool

mapNatBool :: (Nat -> Bool) -> ListNat -> ListBool
mapNatBool = \f xs ->
  case xs of
    NilNat        -> NilBool
    ConsNat n xs' -> ConsBool (f n) (mapNatBool f xs')

any, all :: (Nat -> Bool) -> ListNat -> Bool
any = \p xs -> or  (mapNatBool p xs)
all = \p xs -> and (mapNatBool p xs)

-- task 3: reverse, concat, repeat, cycle

reverse :: ListNat -> ListNat
reverse = \xs ->
  case xs of
    NilNat        -> NilNat
    ConsNat n xs' -> reverse xs' ++ (ConsNat n NilNat)

concat :: ListListNat -> ListNat
concat = \xs ->
  case xs of
    NilListNat         -> NilNat
    ConsListNat as xs' -> as ++ (concat xs')

repeat :: Nat -> Stream
repeat = \x -> Neck x (repeat x)

appendStream :: ListNat -> Stream -> Stream
appendStream = \xs s -> case xs of
  NilNat        -> s
  ConsNat n xs' -> Neck n (appendStream xs' s)

cycle :: ListNat -> Stream
cycle = \xs -> appendStream xs (cycle xs)

-- task 4: iterate, replicate, take, drop

iterate :: (Nat -> Nat) -> Nat -> Stream
iterate = \f x -> Neck x (iterate f (f x))

powersOf2 :: Stream
powersOf2 = iterate (\x -> x * 2) 1

replicate :: Nat -> Nat -> ListNat
replicate = \n a -> takeStream n (repeat a)

takeStream :: Nat -> Stream -> ListNat
takeStream = \n s ->
  case n of
    Zero     -> NilNat
    (Succ a) -> ConsNat (head s) (takeStream a (tail s))

takeListNat :: Nat -> ListNat -> ListNat
takeListNat = \n xs ->
  case n of
    Zero     -> NilNat
    (Succ a) ->
      case xs of
        NilNat          -> NilNat
        (ConsNat n xs') -> ConsNat n (takeListNat a xs')

dropStream :: Nat -> Stream -> Stream
dropStream = \n s ->
  case n of
    Zero     -> s
    (Succ a) -> dropStream a (tail s)

dropListNat :: Nat -> ListNat -> ListNat
dropListNat = \n xs ->
  case n of
    Zero     -> xs
    (Succ a) ->
      case xs of
        NilNat          -> NilNat
        (ConsNat _ xs') -> dropListNat a xs'

-- task 5: takeWhile, dropWhile, null, elem

(>), (<) :: Nat -> Nat -> Bool
(>) = \n m ->
  case n of
    Zero -> False
    (Succ a) ->
      case m of
        Zero -> True
        (Succ b) -> a > b

(<) = \n m -> m > n

(==) = \n m -> not (n < m) && not (n > m)

takeWhileStream :: (Nat -> Bool) -> Stream -> ListNat
takeWhileStream = \p s ->
  case s of
    (Neck x xs) ->
      case p x of
        True  -> ConsNat x (takeWhileStream p xs)
        False -> NilNat

takeWhileListNat :: (Nat -> Bool) -> ListNat -> ListNat
takeWhileListNat = \p xs ->
  case xs of
    NilNat          -> NilNat
    (ConsNat x xs') ->
      case p x of
        True  -> ConsNat x (takeWhileListNat p xs')
        False -> NilNat

dropWhileStream :: (Nat -> Bool) -> Stream -> Stream
dropWhileStream = \p s ->
  case s of
    (Neck x xs) ->
      case p x of
        True  -> dropWhileStream p xs
        False -> s

dropWhileListNat :: (Nat -> Bool) -> ListNat -> ListNat
dropWhileListNat = \p xs ->
  case xs of
    NilNat          -> NilNat
    (ConsNat x xs') ->
      case p x of
        True  -> dropWhileListNat p xs'
        False -> xs

null :: ListNat -> Bool
null = \xs ->
  case xs of
    NilNat        -> True
    (ConsNat _ _) -> False

elem :: Nat -> ListNat -> Bool
elem = \n xs -> not (null (filterListNat (\x -> x == n) xs))

-- task 6: group, nub, partition

data PairListNat = PLN ListNat ListNat

span :: (Nat -> Bool) -> ListNat -> PairListNat
span = \p xs -> PLN (takeWhileListNat p xs) (dropWhileListNat p xs)

group :: ListNat -> ListListNat
group = \xs ->
  case xs of
    NilNat          -> NilListNat
    (ConsNat x xs') ->
      case span (\y -> x == y) xs' of
        PLN g xs'' -> ConsListNat (ConsNat x g) (group xs'')

nub :: ListNat -> ListNat
nub = \xs ->
  case xs of
    NilNat          -> NilNat
    (ConsNat n xs') -> ConsNat n (filterListNat (\m -> not (m == n)) (nub xs'))

data PairStream = PS Stream Stream

psFirst, psSecond :: PairStream -> Stream

psFirst = \ps ->
  case ps of
    (PS a _) -> a

psSecond = \ps ->
  case ps of
    (PS _ b) -> b

partitionStream :: (Nat -> Bool) -> Stream -> PairStream
partitionStream = \p s ->
  case s of
    (Neck x xs) ->
      case partitionStream p xs of
        PS sl sr ->
          case p x of
            True  -> PS (Neck x sl) sr
            False -> PS sl (Neck x sr)

partitionListNat :: (Nat -> Bool) -> ListNat -> PairListNat
partitionListNat = \p xs ->
  case xs of
    NilNat        -> PLN NilNat NilNat
    ConsNat x xs' ->
      case partitionListNat p xs' of
        PLN xsl xsr ->
          case p x of
            True  -> PLN (ConsNat x xsl) xsr
            False -> PLN xsl (ConsNat x xsr)

-- task 7: zip, length

data PairNat = PN Nat Nat
  deriving (Show)

data ListPairNat = NilPairNat | ConsPairNat PairNat ListPairNat

magicList ''ListPairNat

zip :: ListNat -> ListNat -> ListPairNat
zip = \xs ys ->
  case xs of
    NilNat        -> NilPairNat
    ConsNat x xs' ->
      case ys of
        NilNat        -> NilPairNat
        ConsNat y ys' ->
          ConsPairNat (PN x y) (zip xs' ys')

length :: ListNat -> Nat
length = \xs -> sum (mapListNat (\_ -> Succ Zero) xs)

-- task 8: sort

sort :: ListNat -> ListNat
sort = \xs ->
  case xs of
    NilNat           -> NilNat
    ConsNat pivot ts ->
      case partitionListNat (\x -> x < pivot) ts of
        PLN lesser bigger ->
          sort lesser ++ (ConsNat pivot (sort bigger))
