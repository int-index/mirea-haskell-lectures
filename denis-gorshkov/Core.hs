module Core where
-- Подключаем библиотеку Prelude и берем у него Show
import Prelude(Show(..), Int, Num(..), fromIntegral)

data Bool =
  False | True
  deriving(Show)

not :: Bool -> Bool
not = \x ->
  case x of
    False -> True
    True  -> False

-- notnot = id
notnot :: Bool -> Bool
notnot = \x -> not (not x)

id :: Bool -> Bool
id = \x -> x

(&&),(||) :: Bool -> Bool -> Bool

(&&) = \x y ->
  case x of
    False -> False
    True -> y

(||) = \x y ->
  case x of
    False -> y
    True  -> True

-- Succ = Successor(Следующий)
data Nat = Zero | Succ Nat

addNat :: Nat -> Nat -> Nat
addNat = \x y ->
  case x of
    Zero -> y
    (Succ a) -> Succ(addNat a y)

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

--Типы произведения
data Unit = U
data Age = Years Nat
data Point = XY Nat Nat
data Pixel = RGB Nat Nat Nat

data Stream = Neck Nat Stream
  deriving(Show)


zeroes :: Stream
zeroes = Neck Zero zeroes


head :: Stream -> Nat
head = \s ->
  case s of
    (Neck x xs) -> x

tails :: Stream -> Stream
tails = \s ->
  case s of
    (Neck x xs) -> xs

intToNat = \x ->
  case x of
    0 -> Zero
    n -> Succ (intToNat (n-1))

natToInt = \x ->
  case x of
    Zero -> 0
    (Succ a) -> (natToInt a) + 1

nats :: Stream
nats = natsFrom Zero

natsFrom :: Nat -> Stream
natsFrom = \n ->
  Neck n (natsFrom (Succ n))

instance Num Nat where
  (+) = addNat
  fromInteger = \x -> intToNat (fromIntegral x)

instance Show Nat where
  show = \x -> show (natToInt x)


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
        False ->  filterStream p xs
        True -> Neck x(filterStream p xs)

natStream :: Stream
natStream = natStreamFrom Zero

natStreamFrom :: Nat -> Stream
natStreamFrom = \n ->
  Neck n (natStreamFrom (Succ n))

data List = Nil | Cons Nat List

mapList :: (Nat -> Nat) -> (List -> List)
mapList = \f list ->
  case list of
    Nil -> Nil
    (Cons head tail) -> Cons (f head) (mapList f tail)

filterList :: (Nat -> Bool) -> (List -> List)
filterList = \p list ->
  case list of
    Nil -> Nil
    (Cons head tail) ->
      case p head of
        False -> filterList p tail
        True -> Cons head (filterList p tail)

(++) :: List -> List -> List
(++) = \list1 list2 ->
  case list1 of
    Nil -> list2
    (Cons head tail) -> Cons head (tail ++ list2)

multNat :: Nat -> Nat -> Nat
multNat = \first second ->
  case first of
    Zero -> Zero
    Succ Zero -> second
    Succ(a) -> MultNat(a (Succ second))

mult :: Nat -> Nat -> Nat
mult = \x y ->
  case x of
    Zero -> Zero
    (Succ a) -> mult




