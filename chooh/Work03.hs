{-# LANGUAGE OverloadedLists, TemplateHaskell, TypeFamilies, NoImplicitPrelude, UnicodeSyntax #-}

module Work03 where

import Magic

data Nat = Succ Nat | Zero
magicNat ''Nat

data Maybe α = Nothing | Just α
  deriving (Show)

{- Helpers -}

(.) :: (β -> γ) -> (α -> β) -> (α -> γ)
(.) f g x = f (g x)

(++) :: [α] -> [α] -> [α]
xs ++ ys =
  case xs of
    []      -> ys
    n : xs' -> n : (xs' ++ ys)

reverse :: [α] -> [α]
reverse xs =
  case xs of
    [] -> []
    x : xs' -> reverse xs' ++ [x]

{- Homework -}

head :: [α] -> Maybe α
head xs =
  case xs of
    [] -> Nothing
    x : _ -> Just x

tail :: [α] -> Maybe [α]
tail xs =
  case xs of
  [] -> Nothing
  _ : xs' -> Just xs'

last :: [α] -> Maybe α
last = head . reverse

reverseMaybe :: Maybe [α] -> Maybe [α]
reverseMaybe xs =
  case xs of
    Nothing -> Nothing
    Just xs -> Just (reverse xs)

init :: [α] -> Maybe [α]
init = reverseMaybe . tail . reverse
