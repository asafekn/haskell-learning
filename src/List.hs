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
map f xs =
	case xs of
		Node x y -> Node (f x) (map f y)
		Empty -> Empty


tripple :: Int -> Int
tripple a = a * 3


reverse :: List a -> List a
reverse xs =
	case xs of
		Node x y -> concat (reverse y) (Node x (Empty))
		Empty -> Empty


concat :: List a -> List a -> List a
concat xs ys =
	case xs of
		Node x y -> Node (x) (concat y ys)
		Empty -> ys


-- Concat a list between each element of another list
--
--  intercalate [1,2] [ [3,4], [5] ] == [3,4,1,2,5]
--  intercalate "." [ "Asafe", "Klain" ] == "Asafe.Klain"
--

list1 :: List Int
list1 = Node 1 (Node 2 Empty)

list2 :: List (List Int)
list2 = Node (Node 3 (Node 4 Empty)) (Node (Node 5 Empty) Empty)


listSt1 :: List String
listSt1 = (Node "." (Empty))

listSt2 :: List (List String)
listSt2 = (Node (Node "Asafe" (Empty)) (Node (Node "Bastos" (Empty)) (Node (Node "Klain" (Empty)) Empty)))

intercalate :: List a -> List (List a) -> List a
intercalate xs ys =
	case ys of
		Node x y -> concat (concat x xs) (intercalate xs y)
		Empty -> Empty

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
