{-# LANGUAGE TemplateHaskell, NoImplicitPrelude, OverloadedLists, TypeFamilies, FlexibleContexts #-}
module Chalenge where
import Prelude()
import Magic
-- data declaration
data Bool = True | False
    deriving Show

data Nat = Zero | Succ Nat
magicNat ''Nat

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

-- START
-- -- Ch1
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f nil [] = nil
foldr f nil (x : xs) = f x (foldr f nil xs)

foldrSum, foldrProd :: [Nat] -> Nat
foldrOr, foldrAnd :: [Bool] -> Bool
foldrSum = foldr (+) Zero
foldrProd = foldr (*) (Succ Zero)
foldrOr = foldr (||) False
foldrAnd = foldr (&&) True
-- -- Ch2
mapList :: (a -> b) -> ([a]-> [b])
mapList f []=[] 
mapList f (x:xs) = (f x) : (mapList f xs)
-- -- Ch 3
(++) :: [a]->[a]->[a]
(++) xs ys = foldr (:) ys xs
--(++) (x:xs) ys = x:(xs++ys)

