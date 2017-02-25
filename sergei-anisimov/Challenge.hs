{-# LANGUAGE TemplateHaskell #-}

module Challenge where
import Prelude()
import Magic

data Bool =
  False | True
  deriving (Show)

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

-- task 1 - foldr

foldr :: (element -> accum -> accum) ->
         accum ->      -- заменяет на пустой
         [element] ->  --список
         accum
--(not my)
foldr f a list =
  case list of
    []     -> a
    x : xs -> f x (foldr f a xs)

-- task 2 - map
map :: (before   -> after) ->
       ([before] -> [after])

map f list =
  case list of
    [] -> []
    x : xs -> f x : (map f xs)

--map foldr
--map f xs = foldr (\x xs' -> f x : xs') [] xs

-- task 3 - (++)
(++) :: [element] -> [element] -> [element]
(++) list1 list2 =
  --foldr (\ -> ) [] list2
  case list1 of
    []  -> list2
    x1 : xs1 -> x1 : ((++) xs1 list2)
