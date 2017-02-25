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
         [element] ->  -- список
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
  case list1 of
    []  -> list2
    x1 : xs1 -> x1 : ((++) xs1 list2)

-- list1 ++ list2  = foldr (:) list2 list1
-- ++ = flip

-- task 4 - concat
concat :: [[element]] -> [element]
concat = foldr (++) []
{-
concat listOfLists =
  case listOfLists of
    [] -> []
    x : xs ->
      case x of
        [] -> []
        y : ys -> y : (concat (ys : xs))
-}


-- task 5 - filter
filter :: (element -> Bool) -> [element] -> [element]
filter f list =
  case list of
    [] -> []
    x : xs ->
      case f x of
        False -> filter f xs
        True  -> x : (filter f xs)

-- list ввели здесьи сразу применили - поэтому убрали. это ита-редукция.
ffilter p = foldr (
  \x xs -> case p x of
    True  -> x : xs
    False ->     xs
                  ) []

-- task 6 - length
length :: [element] -> Nat
length list =
  case list of
    [] -> 0
    x : xs -> 1 + (length xs)

-- раскрыть подстановкой
flength = foldr (\x n -> 1 + n) 0

-- task 7 - null
null :: [element] -> Bool
null list =
  case list of
    []    -> True
    _ : _ -> False
