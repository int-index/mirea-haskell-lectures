{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module YouTubeStream where

import Prelude()
import Magic

data Bool = True | False
  deriving Show

not :: Bool -> Bool
not = \x -> case x of
  True  -> False
  False -> True

(&&) :: Bool -> Bool -> Bool
(&&) = \x y ->
  case x of
    False -> False
    True  -> y

(||) :: Bool -> Bool -> Bool
(||) = \x y ->
  case x of
    True -> True
    False -> y

data Color = Red | Green | Blue
  deriving Show

niceColor :: Color -> Bool
niceColor = \c -> case c of
  Red -> False
  Green -> True
  Blue -> True


data Unit = U

false :: Unit -> Bool
false = \u -> case u of
  U -> False

data Nat = Zero | Succ Nat

magicNat ''Nat

(+), (*) :: Nat -> Nat -> Nat

(+) = \x y ->
  case x of
    Zero -> y
    (Succ a) -> Succ (a + y)

(*) = \x y ->
  case x of
    Zero     -> Zero
    (Succ a) -> (a * y) + y

one = Succ Zero
two = Succ one

data Point = XY Nat Nat
  deriving Show

data Shape = Rect
  Nat -- x
  Nat -- y
  Nat -- a
  Nat -- b
  deriving Show

data PairPoint = PP Point Point
  deriving Show

boundingBox :: Shape -> PairPoint
boundingBox = \s -> case s of
  Rect x y a b -> PP (XY x y) (XY (x+a) (y+b))

data Stream = Neck Nat Stream

head :: Stream -> Nat
tail :: Stream -> Stream

head = \s ->
  case s of
    (Neck x xs) -> x

tail = \s ->
  case s of
    (Neck x xs) -> xs

zeroes :: Stream
zeroes = Neck Zero zeroes

plusTwo :: Nat -> Nat
plusTwo = (+) 2


natsFrom :: Nat -> Stream
natsFrom = \n ->
  Neck n (natsFrom (Succ n))

nats :: Stream
nats = natsFrom 0

map :: (Nat -> Nat) -> Stream -> Stream
map = \f s ->
  case s of
    (Neck x xs) -> Neck (f x) (map f xs)

threes :: Stream
threes = map (\x -> x + 3) zeroes

evenStream :: Stream
evenStream = map (\x -> x * 2) nats

data List = Nil | Cons Nat List
magicList ''List


mapList :: (Nat -> Nat) -> List -> List
mapList = \f xs ->
  case xs of
    Nil -> Nil
    (Cons x xs') -> Cons (f x) (mapList f xs')

(++) :: List -> List -> List
(++) = \x y ->
  case x of
    Nil -> y
    (Cons e a) -> Cons e (a ++ y)

{-
fold (+) 0 [1,2,3] = 1 + 2 + 3 + 0
fold (*) 1 [1,2,3] = 1 * 2 * 3 * 1
-}


idList :: List -> List
idList = foldList Cons Nil

sum :: List -> Nat
sum = foldNat (+) 0

{-
idList (Cons 1 (Cons 2 (Cons 3 Nil))) =
Cons 1 (idList (Cons 2 (Cons 3 Nil))) =
Cons 1 (Cons 2 (idList (Cons 3 Nil))) =
Cons 1 (Cons 2 (Cons 3 (idList Nil))) =
Cons 1 (Cons 2 (Cons 3 Nil))
-}

foldList :: (Nat -> List -> List) -> List -> List -> List
foldList = \cons nil xs ->
  case xs of
    Nil -> nil
    Cons x xs' -> cons x (foldList cons nil xs')

foldNat :: (Nat -> Nat -> Nat) -> Nat -> List -> Nat
foldNat = \cons nil xs ->
  case xs of
    Nil -> nil
    Cons x xs' -> cons x (foldNat cons nil xs')
