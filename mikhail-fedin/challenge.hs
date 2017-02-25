{-# LANGUAGE TemplateHaskell #-}
module Challenge where

import Prelude()
import Magic

data Nat = Succ Nat | Zero

magicNat ''Nat

data Bool = True | False
  deriving Show

(+), (*) :: Nat -> Nat -> Nat
(+) = \x y ->
    case x of
      Zero -> y
      (Succ x') -> Succ (x' + y)

(*) = \x y ->
    case x of
      Zero -> Zero
      (Succ x') -> y + (x' * y)

foldr :: (element -> accum -> accum) ->
 accum -> [element] -> accum
foldr accum element list =
  case list of
    [] -> element
    (element' : list') -> accum element' (foldr accum element list')
