module List where

import Prelude hiding (length, map, reverse, concat, fold)

data List a = Empty | Node a (List a)
	deriving (Show, Eq)

length :: List a -> Int
length xs =
	case xs of
		Node a b -> 1 + length b
		Empty -> 0

map :: (a -> b) -> List a -> List b
map = undefined

concat :: List a -> List a -> List a
concat = undefined

reverse :: List a -> List a
reverse = undefined

-- Concat a list between each element of another list
--
--  intercalate [1,2] [ [3,4], [5] ] == [3,4,1,2,5]
--  intercalate "." [ "Asafe", "Klain" ] == "Asafe.Klain"
--
intercalate :: List a -> List (List a) -> List a

--
--   fold (\x y -> x + y) 5 (Node 1 (Node 2 Empty))
--   fold (\x y -> x + y) ((\x y -> x + y) 1 5) (Node 2 Empty)
--   fold (\x y -> x + y) 6 (Node 2 Empty)
--   fold (\x y -> x + y) ((\x y -> x + y) 2 6) Empty
--   fold (\x y -> x + y) 8 Empty
--   8
fold :: (a -> b -> b) -> b -> List a -> b
fold f acc xs =
	case xs of
		Node y ys -> fold f (f y acc) ys
		Empty -> acc
