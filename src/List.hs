module List where

import Prelude hiding ((<>), (:))

data IntList = Empty | (:::) Int IntList
  deriving (Show, Eq)

infixr 8 :::
infixr 2 <>

data MaybeInt = NoInt | JustInt Int
  deriving (Show, Eq)

twoNumbers :: IntList
twoNumbers = 1 ::: 2 ::: Empty

anotherTwoNumbers :: IntList
anotherTwoNumbers = 3 ::: (4 ::: Empty)

intHead :: IntList -> MaybeInt
intHead list =
	case list of
		n ::: xs -> JustInt n
		Empty -> NoInt

add1 :: Int -> Int
add1 a = a + 1

tripple :: Int -> Int
tripple a = a * 3

f :: Int -> (Int -> Int)
f x =
  if x > 10
  then add1
  else tripple

lengthIntList :: IntList -> Int
lengthIntList list =
	case list of
		a ::: b -> (1 + lengthIntList b)
		Empty -> 0

-- Get the last element of the list
--
--    lastIntList (1 ::: (2 ::: Empty)) == JustInt 2
--
lastIntList :: IntList -> MaybeInt
lastIntList list =
	case list of
		a ::: b -> case b of
			c ::: d -> lastIntList b
			Empty -> JustInt a

		Empty -> NoInt

-- Apply function f to every element of the list
--
--     mapIntList tripple (1 ::: (2 ::: Empty)) == 3 ::: (6 ::: Empty)
--
mapIntList :: (Int -> Int) -> IntList -> IntList
mapIntList f xs =
	case xs of
		a ::: b -> f a ::: mapIntList f b
		Empty -> Empty

-- Merge two IntLists
--
--  concatIntList (1 ::: (2 ::: Empty)) (3 ::: (4 ::: Empty)) ==
--      1 ::: (2 ::: (3 ::: (4 ::: Empty)))
--
concatIntList :: IntList -> IntList -> IntList
concatIntList xs ys =
	case xs of
		a ::: b -> a ::: (concatIntList b ys)
		Empty -> ys

(<>) :: IntList -> IntList -> IntList
(<>) = concatIntList

-- Return the reversed list
--
--  reverseIntList (1 ::: (2 ::: Empty)) == 2 ::: (1 ::: Empty)
--
reverseIntList :: IntList -> IntList
reverseIntList xs =
	case xs of
		a ::: b -> concatIntList (reverseIntList b) (a ::: Empty)
		Empty -> Empty
