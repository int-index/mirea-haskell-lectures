{-# LANGUAGE TemplateHaskell, NoImplicitPrelude #-}

module Challenge where
import Prelude()

import Magic 

data Bool = False | True 
    deriving (Show)

data Nat = Succ Nat | Zero 
magicNat ''Nat

-- data List element = Nil | Cons element (List element)
-- data [element] = [] | (:) element [element]

(+), (*) :: Nat -> Nat -> Nat

x + y =
  case x of
    Zero   -> y
    Succ a -> Succ (a + y)

x * y =
  case x of
    Zero   -> Zero
    Succ a -> y + (a * y)


foldr :: (element -> accum -> accum) ->
          accum ->
          [element] ->
          accum 

foldr cons nil xs = 
    case xs of
        [] -> nil
        (x:xs') -> cons x (foldr cons nil xs')

map :: (before -> after) -> ([before] -> [after])

map f (xs) = 
    foldr (\y ys -> (f y):ys) [] xs

(.) :: (beta -> gamma) -> (alpha -> beta) -> (alpha -> gamma)
f . g = \x -> f (g x)


flip :: (alpha -> beta  -> gamma) ->
        (beta  -> alpha -> gamma)
flip = \f x y -> f y x

(++) :: [element] -> [element] -> [element]

--xs ++ ys = 
  --  flip (foldr (:)) xs ys

(++) = flip (foldr(:))
