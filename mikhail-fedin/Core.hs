{-# LANGUAGE OverloadedLists, TemplateHaskell, TypeFamilies, NoImplicitPrelude #-}
module Core where

import Magic
import Prelude()

data Nat = Zero | Succ Nat

magicNat ''Nat

const :: a -> b -> a
const x _ = x

id :: a -> a
id x = x

flip :: (a -> b -> c) -> b -> a -> c
flip f x y = f y x

($) :: (a -> b) -> a -> b
($) = id
infixr 0 $

(.) :: (b -> c) -> (a -> b) -> a -> c
(.) f g = \x -> f (g x)
infixr .


data Bool = True | False
  deriving Show

not :: Bool -> Bool
not = \x ->
  case x of
    True -> False
    False -> True

(&&), (||) :: Bool -> Bool -> Bool
(&&) = \x y ->
  case x of
    False -> False
    True -> y
(||) = \x y ->
  case x of
    False -> y
    True -> True

(+), (*) :: Nat -> Nat -> Nat
(+) = \x y ->
  case x of
    Zero -> y
    (Succ x') -> Succ (x' + y)

(*) = \x y ->
  case x of
    Zero -> Zero
    (Succ x') -> y + (x' * y)

odd, even :: Nat -> Bool
odd = \x ->
  case x of
    Zero -> True
    (Succ a) -> even a
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
(<)  = flip (>)
(>=) = \x y -> not (x < y)
(==) = \x y -> (x >= y) && (x <= y)
(/=) = \x y -> not (x == y)

data Stream a = a :> Stream a
  deriving Show

infixr :>

headStream :: Stream a -> a
headStream xs =
  case xs of
    (x :> _) -> x

tailStream :: Stream a -> Stream a
tailStream xs =
  case xs of
    (_ :> xs') -> xs'

repeat :: a -> Stream a
repeat x = x :> repeat x

iterate :: (a -> a) -> a -> Stream a
iterate f x = x :> iterate f (f x)

(!!) :: Stream a -> Nat -> a
(!!) s x =
  case x of
    Zero -> headStream s
    (Succ n) -> tailStream s !! n

mapStream :: (a -> b) -> Stream a -> Stream b
mapStream f s =
  case s of
    (x :> s') -> f x :> mapStream f s'

filterStream :: (a -> Bool) -> Stream a -> Stream a
filterStream p s =
  case s of
    (x :> s') ->
      case p x of
        True -> x :> filterStream p s'
        False -> filterStream p s'

streamToList :: Stream a -> [a]
streamToList s =
  case s of
    (x :> s) -> x : (streamToList s)
 
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f x xs =
  case xs of
    [] -> x
    (x':xs') -> f x' (foldr f x xs')

foldl :: (b -> a -> b) -> b -> [a] -> b
foldl f x xs =
  case xs of
    [] -> x
    (x':xs') -> foldl f (f x x') xs'

(++) :: [a] -> [a] -> [a]
(++) xs ys = foldr (:) ys xs

map :: (a -> b) -> [a] -> [b]
map f xs =
  case xs of
    [] -> []
    (x:xs') -> (f x) : (map f xs')

sum, product :: [Nat] -> Nat
sum = foldr (+) Zero
product = foldr (*) (Succ Zero)

and, or :: [Bool] -> Bool
and = foldr (&&) True
or = foldr (||) False

all, any :: (Nat -> Bool) -> [Nat] -> Bool
all p xs = and (map p xs)
any p xs = or  (map p xs)

concat :: [[a]] -> [a]
concat = foldr (++) []

reverse :: [a] -> [a]
reverse = foldl (flip (:)) []

prepend :: [a] -> Stream a -> Stream a
prepend xs s = foldr (:>) s xs

cycle :: [a] -> Stream a
cycle xs = prepend xs (cycle xs)

take :: Nat -> [a] -> [a]
take n xs =
  case n of
    Zero -> []
    (Succ n') ->
      case xs of
        [] -> []
        (x:xs') -> x : (take n' xs')

takeStream :: Nat -> Stream a -> [a]
takeStream n s = take n (streamToList s)

replicate :: Nat -> a -> [a]
replicate n x = takeStream n (repeat x)

filter :: (a -> Bool) -> [a] -> [a]
filter p xs =
  case xs of
    [] -> []
    (x:xs') ->
      case p x of
        False -> filter p xs'
        True -> x : (filter p xs')

takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile p xs =
  case xs of
    [] -> []
    (x:xs') ->
      case p x of
        False -> []
        True -> x : (takeWhile p xs')

takeWhileStream :: (a -> Bool) -> Stream a -> [a]
takeWhileStream p s = takeWhile p (streamToList s)

dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile p xs =
  case xs of
    [] -> []
    (x:xs') ->
      case p x of
        False -> x:xs'
        True -> dropWhile p xs'

dropWhileStream :: (a -> Bool) -> Stream a -> Stream a
dropWhileStream p xs =
  case xs of
    (x :> xs') ->
      case p x of
        False -> x :> xs'
        True -> dropWhileStream p xs'

null :: [a] -> Bool
null xs =
  case xs of
    [] -> True
    _ -> False

elem :: Nat -> [Nat] -> Bool
elem = \x xs -> or (map (==x) xs)

groupBy :: (a -> a -> Bool) ->  [a] -> [[a]]
groupBy f xs =
  case xs of
    [] -> []
    (x:xs') -> (x : takeWhile (f x) xs') : (groupBy f (dropWhile (f x) xs'))
 
group :: [Nat] -> [[Nat]]
group = groupBy (==)

nub :: [Nat] -> [Nat]
nub xs =
  case xs of
    [] -> []
    (x:xs') -> x : (nub (filter (/=x) xs'))

-- Альтернативный вариант nub
nub' :: [Nat] -> [Nat]
nub' = nub_ []

nub_ :: [Nat] -> [Nat] -> [Nat]
nub_ acc xs =
  case xs of
    [] -> reverse acc
    (x:xs') ->
      case elem x acc of
        True -> nub_ acc xs'
        False -> nub_ (x:acc) xs'

data Pair a b = Pair a b
  deriving Show

partition :: (a -> Bool) -> [a] -> Pair [a] [a]
partition p xs = Pair (filter p xs) (filter (\x -> not (p x)) xs)

zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith f xs ys =
  case xs of
    [] -> []
    (x:xs') ->
      case ys of
        [] -> []
        (y:ys') -> (f x y) : (zipWith f xs' ys')

zip :: [a] -> [b] -> [Pair a b]
zip = zipWith Pair

length :: [a] -> Nat
length = foldr (const Succ) Zero

--rangeStep :: Nat -> Nat -> Nat -> [Nat]
--rangeStep a b c = takeWhileStream (<=c) (iterate (+ (b-a)) a)

--range :: Nat -> Nat -> [Nat]
--range a b = rangeStep a (Succ a) b

sortBy :: (a -> a -> Bool) -> [a] -> [a]
sortBy f xs =
  case xs of
    []          -> []
    (pivot:xs') ->
      case partition (flip f pivot) xs' of
        Pair lesser bigger ->
          (sortBy f lesser) ++ (pivot : (sortBy f bigger))

sort :: [Nat] -> [Nat]
sort = sortBy (<)

data Maybe a = Nothing | Just a
  deriving Show

head :: [a] -> Maybe a
head xs =
  case xs of
    [] -> Nothing
    (x : _) -> Just x
 
tail :: [a] -> Maybe [a]
tail xs =
  case xs of
    [] -> Nothing
    (_ : xs') -> Just xs'

init :: [a] -> Maybe [a]
init xs = 
  case tail (reverse xs) of
    Nothing -> Nothing
    Just xs' ->  Just (reverse xs')

last :: [a] -> Maybe a
last xs = head (reverse xs)


(-) :: Nat -> Nat -> Maybe Nat
(-) x y =
  case x of
    Zero ->
      case y of
        Zero -> Just Zero
        _ -> Nothing
    (Succ x') ->
      case y of
        (Succ y') -> x' - y'
        Zero -> Just x


divMod :: Nat -> Nat -> Maybe (Pair Nat Nat)
divMod x y =
  case y of
    Zero -> Nothing
    _ ->
      case x - y of
        Nothing -> Just (Pair 0 x)
        Just x' ->
          case divMod x' y of
            Nothing -> Nothing
            Just (Pair n a) -> Just (Pair (n+1) a)

div :: Nat -> Nat -> Maybe Nat
div x y =
  case divMod x y of
    Nothing -> Nothing
    Just (Pair a _) -> Just a

mod :: Nat -> Nat -> Maybe Nat
mod x y =
  case divMod x y of
    Nothing -> Nothing
    Just (Pair _ b) -> Just b

maybe :: b -> (a -> b) -> Maybe a -> b
maybe b f m =
  case m of
    Nothing -> b
    Just a -> f a

fromMaybe :: a -> Maybe a -> a
fromMaybe b = maybe b id 

mmap :: (a -> b) -> Maybe a -> Maybe b
mmap f = maybe Nothing (Just . f)


catMaybes :: [Maybe a] -> [a]
catMaybes xs = foldr (maybe id (:)) [] xs

mapMaybes :: (a -> Maybe b) -> [a] -> [b]
mapMaybes f = catMaybes . map f
