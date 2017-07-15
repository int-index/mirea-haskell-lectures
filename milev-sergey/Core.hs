{-# LANGUAGE TemplateHaskell, NoImplicitPrelude, OverloadedLists, TypeFamilies, FlexibleContexts #-}
module Core where

import Magic

-- -- data Types derlaretions
data Bool = True | False
    deriving Show

data Nat = Zero | Succ Nat
magicNat ''Nat

data Stream a = a :> Stream a
    deriving Show
infixr :>

data Maybe a = Nothing | Just a
    deriving Show

data Pair a b = P a b
    deriving Show
-- Bool functions
not :: Bool -> Bool
not False = True
not True = False

notnot :: Bool -> Bool
notnot a = not (not a)

id :: a -> a 
id a = a

const :: a -> b -> a
const a b = a

(&&),(||) :: Bool -> Bool -> Bool
(&&) True a = a
(&&) _ _ = False
(||) False a = a
(||) _ _ = True

-- Nat functions
(+) :: Nat -> Nat -> Nat
(+) Zero b = b
(+) (Succ a) b = Succ ((+) a b)

(*)::Nat->Nat -> Nat
(*) Zero b = Zero
(*) (Succ a) b = (a * b) + b

even,odd :: Nat -> Bool
even Zero = True
even (Succ a) = odd a
odd Zero = False
odd (Succ a) = even a

-- -- Stream functions
-- Stream Nat functions
zeroes :: Stream Nat
zeroes = Zero :> zeroes

ones :: Stream Nat
ones = Succ Zero :> ones

natsFrom :: Nat -> Stream Nat
natsFrom n = n :> natsFrom (Succ n)

natsFromZero :: Stream Nat
natsFromZero = natsFrom Zero

-- Stream polymorphic functions
headStream::Stream a -> a
headStream (x:>xs) = x

tailStream :: Stream a -> Stream a
tailStream (x:>xs) = xs

repeatStream :: a -> Stream a
repeatStream n = n :> (repeatStream n)

mapStream :: (a-> b) -> (Stream a -> Stream b)
mapStream f (x:>xs) =  (f x) :> (mapStream f xs)

filterStream :: (a -> Bool)->(Stream a ->Stream a)
filterStream f (x :> xs)= 
            case f x of
                False -> filterStream f xs
                True -> (x :> (filterStream f xs))
iterate :: (a -> a) -> a -> Stream a
iterate f n =  n :> iterate f (f n)

takeStream :: Nat -> Stream a -> [a]
takeStream Zero s = []
takeStream (Succ a) s = headStream s : takeStream a (tailStream s)

dropStream :: Nat -> Stream a -> Stream a
dropStream Zero s = s
dropStream (Succ a) s = dropStream a (tailStream s)
-- polymorthic List Functions

filterList :: (a ->Bool) -> ([a]->[a])
filterList f [] = []
filterList f (x:xs) =
            case f x of
                False -> filterList f xs
                True -> (x:(filterList f xs))

mapList :: (a -> b) -> ([a]-> [b])
mapList f []=[] 
mapList f (x:xs) = (f x) : (mapList f xs)

(++) :: [a]->[a]->[a]
(++) [] ys = ys
(++) (x:xs) ys = x:(xs++ys)

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f nil [] = nil
foldr f nil (x : xs) = f x (foldr f nil xs)

foldrSum, foldrProd :: [Nat] -> Nat
foldrOr, foldrAnd :: [Bool] -> Bool
foldrSum = foldr (+) Zero
foldrProd = foldr (*) (Succ Zero)
foldrOr = foldr (||) False
foldrAnd = foldr (&&) True

all,any :: (t->Bool)->[t] -> Bool
all f xs = foldrAnd (mapList f xs) 
any f xs = foldrOr (mapList f xs) 

revers :: [a] -> [a]
revers [] = []
revers (x : xs) = revers xs ++ ( x : [])

concat :: [[a]] -> [a]
concat [] = []
concat (x : xs) = x ++ (concat xs) 

appendStream :: [a] -> Stream a -> Stream a
appendStream [] s = s
appendStream (x : xs) s = x :> (appendStream xs s)

cycle :: [a] -> Stream a
cycle xs = appendStream xs (cycle xs)

replicate :: Nat -> a -> [a]
replicate n a = takeStream n (repeatStream a)

takeList :: Nat -> [a] -> [a]
takeList Zero _ = []
takeList (Succ a) [] = []
takeList (Succ a) (x : xs) = x : takeList a xs

dropList :: Nat -> [a] -> [a]
dropList Zero xs = xs
dropList (Succ a) [] = []
dropList (Succ a) (_ : xs) =  dropList a xs

-- zip, lenght (83)
zip :: [a] -> [b] -> [Pair a b]
zip [] [] = []
zip (x:xs) (y:ys) = (P x y) : (zip xs ys)

len :: [a] -> Nat
len [] = Zero
len (_:xs) = 1+len xs
-- (>), (<), (==), (<=),(>=) (84)
(>), (<), (==), (<=),(>=) :: Nat -> Nat -> Bool
(>) Zero Zero = False 
(>) _ Zero = True
(>) Zero _ = False
(>) (Succ a) (Succ b) = (>) a b

(>=) Zero Zero = True
(>=) _ Zero = True
(>=) Zero _ = False
(>=) (Succ a) (Succ b) = (>=) a b

flip :: (a -> b -> c) -> (b -> a -> c)
flip f a b = f b a

(<) = flip (>)
(<=) = flip (>=)
(==) a b = not (a<b) && not (a>b)

-- Maybe functions
head :: [a] -> Maybe a
head [] = Nothing
head (x : xs) = Just x

tail :: [a] -> Maybe [a]
tail [] = Nothing
tail (x : xs) = Just xs

last :: [a] -> Maybe a
last [] = Nothing
last xs = head (revers xs)

init :: [a] -> Maybe [a]
init xs = 
  case tail (revers xs) of
    Nothing -> Nothing
    Just xs' ->  Just (revers xs')

(-) :: Nat -> Nat -> Maybe Nat 
(-) = \x y ->
    case x of
        Zero ->
            case y of
                Zero -> Just Zero
                _ -> Nothing
        (Succ a) ->
            case y of
                Zero -> Just x
                (Succ b) -> a - b
divMod :: Nat -> Nat -> Maybe (Pair Nat Nat)
divMod = \x y ->
    case y of
        Zero -> Nothing
        _ ->
            case x - y of
                Nothing -> Just (P 0 x)
                Just a ->
                    case divMod a y of
                        Nothing -> Nothing
                        Just (P b c) -> Just (P (b+1) c)
