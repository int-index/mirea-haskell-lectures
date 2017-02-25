{-# LANGUAGE TemplateHaskell, NoImplicitPrelude #-}

module Core where

import Magic

data Bool =
  False | True
  deriving (Show)

not :: Bool -> Bool
not x =
  case x of
    False -> True
    True  -> False

notnot :: Bool -> Bool
notnot = \x -> not (not x)

id :: alpha -> alpha
id = \x -> x

const :: alpha -> beta -> alpha
const = \a b -> a

(&&), (||) :: Bool -> Bool -> Bool

x && y =
  case x of
    False -> False
    True  -> y

x || y =
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

x + y =
  case x of
    Zero   -> y
    Succ a -> Succ (a + y)

x * y =
  case x of
    Zero   -> Zero
    Succ a -> y + (a * y)

even, odd :: Nat -> Bool

even x =
  case x of
    Zero   -> True
    Succ a -> odd a

odd x =
  case x of
    Zero   -> False
    Succ a -> even a

data Age = Years Nat

data Point = XY Nat Nat

data Pixel = RGB Nat Nat Nat

data Shape = Rect Nat Nat Nat Nat

data Stream alpha = alpha :> Stream alpha

infixr :>

zeroes :: Stream Nat
zeroes = Zero :> zeroes

head :: Stream alpha -> alpha
head s =
  case s of
    x :> xs -> x

tail :: Stream alpha -> Stream alpha
tail s =
  case s of
    x :> xs -> xs

nats :: Stream Nat
nats = natsFrom Zero

natsFrom :: Nat -> Stream Nat
natsFrom n = n :> natsFrom (Succ n)

mapStream :: (alpha -> beta) -> Stream alpha -> Stream beta
mapStream f s =
  case s of
    x :> xs -> f x :> mapStream f xs

ones :: Stream Nat
ones = mapStream Succ zeroes

filterStream :: (alpha -> Bool) -> Stream alpha -> Stream alpha
filterStream p s =
  case s of
    x :> xs ->
      case p x of
        False ->      filterStream p xs
        True  -> x :> filterStream p xs

filterList :: (alpha -> Bool) -> [alpha] -> [alpha]
filterList p s =
  case s of
    []     -> []
    x : xs ->
      case p x of
        False ->     filterList p xs
        True  -> x : filterList p xs

(++) :: [alpha] -> [alpha] -> [alpha]
xs ++ ys =
  case xs of
    []      -> ys
    n : xs' -> n : (xs' ++ ys)

-- Homework

-- task 1: foldNat, foldBool

sum, product :: [Nat] -> Nat
sum     = foldr (+) Zero
product = foldr (*) (Succ Zero)

and, or :: [Bool] -> Bool
and = foldr (&&) True
or  = foldr (||) False

-- task 2: map

map :: (alpha -> beta) -> [alpha] -> [beta]
map f xs =
  case xs of
    []      -> []
    a : xs' -> f a : map f xs'

any, all :: (alpha -> Bool) -> [alpha] -> Bool
any p = or . map p
all p = and . map p

-- task 3: reverse, concat, repeat, cycle

reverse :: [alpha] -> [alpha]
reverse xs =
  case xs of
    []      -> []
    n : xs' -> reverse xs' ++ [n]

concat :: [[alpha]] -> [alpha]
concat = foldr (++) []

repeat :: alpha -> Stream alpha
repeat x = x :> repeat x

appendStream :: [alpha] -> Stream alpha -> Stream alpha
appendStream xs s =
  case xs of
    []      -> s
    n : xs' -> n :> (appendStream xs' s)

cycle :: [alpha] -> Stream alpha
cycle xs = appendStream xs (cycle xs)

-- task 4: iterate, replicate, take, drop

iterate :: (alpha -> alpha) -> alpha -> Stream alpha
iterate f x = x :> iterate f (f x)

powersOf2 :: Stream Nat
powersOf2 = iterate (*2) 1

replicate :: Nat -> alpha -> [alpha]
replicate n a = takeStream n (repeat a)

takeStream :: Nat -> Stream alpha -> [alpha]
takeStream n s =
  case n of
    Zero   -> []
    Succ a -> head s : takeStream a (tail s)

takeList :: Nat -> [alpha] -> [alpha]
takeList n xs =
  case n of
    Zero   -> []
    Succ a ->
      case xs of
        []      -> []
        n : xs' -> n : takeList a xs'

dropStream :: Nat -> Stream a -> Stream a
dropStream n s =
  case n of
    Zero   -> s
    Succ a -> dropStream a (tail s)

dropList :: Nat -> [alpha] -> [alpha]
dropList n xs =
  case n of
    Zero   -> xs
    Succ a ->
      case xs of
        []      -> []
        _ : xs' -> dropList a xs'

-- task 5: takeWhile, dropWhile, null, elem

(>), (<) :: Nat -> Nat -> Bool
n > m =
  case n of
    Zero   -> False
    Succ a ->
      case m of
        Zero   -> True
        Succ b -> a > b

flip :: (alpha -> beta  -> gamma) ->
        (beta  -> alpha -> gamma)
flip = \f x y -> f y x

(<) = flip (>)

n == m = not (n < m) && not (n > m)

takeWhileStream :: (alpha -> Bool) -> Stream alpha -> [alpha]
takeWhileStream p s =
  case s of
    x :> xs ->
      case p x of
        True  -> x : takeWhileStream p xs
        False -> []

takeWhileList :: (alpha -> Bool) -> [alpha] -> [alpha]
takeWhileList p xs =
  case xs of
    []      -> []
    x : xs' ->
      case p x of
        True  -> x : takeWhileList p xs'
        False -> []

dropWhileStream :: (alpha -> Bool) -> Stream alpha -> Stream alpha
dropWhileStream p s =
  case s of
    x :> xs ->
      case p x of
        True  -> dropWhileStream p xs
        False -> s

dropWhileList :: (alpha -> Bool) -> [alpha] -> [alpha]
dropWhileList p xs =
  case xs of
    []      -> []
    x : xs' ->
      case p x of
        True  -> dropWhileList p xs'
        False -> xs

null :: [alpha] -> Bool
null xs =
  case xs of
    []    -> True
    _ : _ -> False

elem :: Nat -> [Nat] -> Bool
elem n = not . null . filterList (== n)

-- task 6: group, nub, partition

span :: (alpha -> Bool) -> [alpha] -> Pair [alpha] [alpha]
span p xs = P (takeWhileList p xs) (dropWhileList p xs)

group :: [Nat] -> [[Nat]]
group xs =
  case xs of
    []      -> []
    x : xs' ->
      case span (x ==) xs' of
        P g xs'' -> (x : g) : group xs''

nub :: [Nat] -> [Nat]
nub xs =
  case xs of
    []      -> []
    n : xs' -> n : filterList (\m -> not (m == n)) (nub xs')

partitionStream ::
  (alpha -> Bool) ->
  Stream alpha ->
  Pair (Stream alpha) (Stream alpha)
partitionStream p s =
  case s of
    x :> xs ->
      case partitionStream p xs of
        P sl sr ->
          case p x of
            True  -> P (x :> sl) sr
            False -> P sl (x :> sr)

partitionList ::
  (alpha -> Bool) ->
  [alpha] ->
  Pair [alpha] [alpha]
partitionList p xs =
  case xs of
    []      -> P [] []
    x : xs' ->
      case partitionList p xs' of
        P xsl xsr ->
          case p x of
            True  -> P (x : xsl) xsr
            False -> P xsl (x : xsr)

-- task 7: zip, length

data Pair alpha beta = P alpha beta
  deriving Show

fst :: Pair alpha beta -> alpha
fst p = case p of
  P a _ -> a

snd :: Pair alpha beta -> beta
snd p = case p of
  P _ b -> b

zip :: [alpha] -> [beta] -> [Pair alpha beta]
zip xs ys =
  case xs of
    []      -> []
    x : xs' ->
      case ys of
        []      -> []
        y : ys' ->
          P x y : zip xs' ys'

length :: [alpha] -> Nat
length = foldr (const Succ) Zero

-- task 8: sort

sort :: [Nat] -> [Nat]
sort xs =
  case xs of
    []         -> []
    pivot : ts ->
      case partitionList (< pivot) ts of
        P lesser bigger ->
          sort lesser ++ (pivot : sort bigger)

(.) :: (beta -> gamma) -> (alpha -> beta) -> (alpha -> gamma)
f . g = \x -> f (g x)

infixr .

foldr :: (alpha -> beta -> beta) -> beta -> [alpha] -> beta
foldr cons nil xs =
  case xs of
    []      -> nil
    (x:xs') -> cons x (foldr cons nil xs')

compose :: [alpha -> alpha] -> alpha -> alpha
compose = foldr (.) id
