{-# LANGUAGE TemplateHaskell, NoImplicitPrelude, OverloadedLists, TypeFamilies, FlexibleContexts #-}
module Core where

import Magic

-- -- data Types derlaretions
data Bool = True | False
    deriving Show

data Nat = Zero | Succ Nat
magicNat ''Nat

data Stream = Neck Nat Stream
    deriving Show

-- List data types
data ListNat = NilNat | ConsNat Nat ListNat
magicList ''ListNat

data ListBool = NilBool | ConsBool Bool ListBool
magicList ''ListBool

data ListListNat = NilListNat | ConsListNat ListNat ListListNat
magicList ''ListListNat

-- Bool functions
not :: Bool -> Bool
not False = True
not True = False

notnot :: Bool -> Bool
notnot a = not (not a)

id :: Bool -> Bool
id a = a

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

-- Stream functions
zeroes :: Stream
zeroes = Neck Zero zeroes

head::Stream->Nat
head (Neck x xs) = x

tail :: Stream -> Stream
tail (Neck x xs) = xs

natsFrom :: Nat -> Stream
natsFrom n = Neck n (natsFrom (Succ n))

repeat :: Nat -> Stream
repeat n = Neck n (repeat n)

natsFromZero :: Stream
natsFromZero = natsFrom Zero

map :: (Nat -> Nat) -> (Stream -> Stream)
map f (Neck x xs) = Neck (f x) (map f xs)

filter :: (Nat->Bool)->(Stream->Stream)
filter f (Neck x xs)= 
            case f x of
                False -> filter f xs
                True -> (Neck x (filter f xs))
-- -- List functions
-- ListNat functions
mapListNat :: (Nat -> Nat) -> (ListNat -> ListNat)
mapListNat f NilNat = NilNat
mapListNat f (ConsNat x xs) = ConsNat (f x) (mapListNat f xs)

filterListNat :: (Nat->Bool) -> (ListNat->ListNat)
filterListNat f NilNat = NilNat
filterListNat f (ConsNat x xs) =
            case f x of
                False -> filterListNat f xs
                True -> (ConsNat x (filterListNat f xs))

(++) :: ListNat->ListNat->ListNat
(++) NilNat ys = ys
(++) (ConsNat x xs) ys = ConsNat x (xs++ys)

foldNat :: (Nat -> Nat -> Nat) -> Nat -> ListNat -> Nat
foldNat f nil NilNat = nil 
foldNat f nil (ConsNat x xs) = f x (foldNat f nil xs) 

foldNatSum, foldNatProd :: ListNat -> Nat
foldNatSum = foldNat (+) Zero
foldNatProd = foldNat (*) (Succ Zero)
-- ListBool functions
foldBool :: (Bool -> Bool -> Bool) -> Bool -> ListBool -> Bool
foldBool f nil NilBool = nil
foldBool f nil (ConsBool x xs) = f x (foldBool f nil xs)

foldBoolAnd, foldBoolOr :: ListBool -> Bool
foldBoolAnd = foldBool (&&) True
foldBoolOr = foldBool (||) False
-- mixed List functions
mapNatBool :: (Nat -> Bool) -> ListNat -> ListBool
mapNatBool f NilNat = NilBool
mapNatBool f (ConsNat x xs) = ConsBool (f x) (mapNatBool f xs)

all,any :: (Nat->Bool)->ListNat -> Bool
all f xs = foldBoolAnd (mapNatBool f xs) 
any f xs = foldBoolOr (mapNatBool f xs) 

revers :: ListNat->ListNat
revers NilNat = NilNat
revers (ConsNat x xs) = revers xs ++ (ConsNat x NilNat)

concat :: ListListNat -> ListNat
concat NilListNat = NilNat
concat (ConsListNat x xs) = x ++ (concat xs)

appendStream :: ListNat -> Stream -> Stream
appendStream NilNat s = s
appendStream (ConsNat x xs) s = Neck x (appendStream xs s)

cycle :: ListNat -> Stream
cycle xs = appendStream xs (cycle xs)
