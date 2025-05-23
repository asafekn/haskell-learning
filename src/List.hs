module List where

import Prelude hiding (length, map, reverse, concat, fold)

length :: [a] -> Int
length xs =
	case xs of
		_ : xs' -> 1 + length xs'
		[] -> 0


map :: (a -> b) -> [a] -> [b]
map f xs =
	case xs of
		x : xs' -> f x : map f xs'
		[] -> []


tripple :: Int -> Int
tripple a = a * 3


reverse :: [a] -> [a]
reverse xs =
	case xs of
		x : xs' -> concat (reverse xs') [x]
		[] -> []


concat :: [a] -> [a] -> [a]
concat xs ys =
	case xs of
		x : xs' -> x : concat xs' ys
		[] -> ys


-- Concat a list between each element of another list
--
--  intercalate [1,2] [ [3,4], [5] ] == [3,4,1,2,5]
--  intercalate "." [ "Asafe", "Klain" ] == "Asafe.Klain"
--


intercalate :: [a] -> [[a]] -> [a]
intercalate xs ys =
	case ys of
    y : [] -> y
    y : ys' -> concat (concat y xs) (intercalate xs ys')
    [] -> []

--  intercalate "." [ "Asafe", "Klain" ]
--  concat (concat "Asafe" ".") (intercalate "." ["Klain"])
--  concat ("Asafe.") (concat (concat "Klain" ".") (intercalate "." []))
--  concat "Asafe." (concat "Klain." (intercalate "." []))
--  concat "Asafe." (concat "Klain." [])
--  concat "Asafe." "Klain."
--  "Asafe.Klain."
--
--
--

--
--   fold (\x y -> x + y) 5 (Node 1 (Node 2 []))
--   fold (\x y -> x + y) ((\x y -> x + y) 1 5) (Node 2 [])
--   fold (\x y -> x + y) 6 (Node 2 [])
--   fold (\x y -> x + y) ((\x y -> x + y) 2 6) []
--   fold (\x y -> x + y) 8 []
--   8
fold :: (a -> b -> b) -> b -> [a] -> b
fold f acc xs =
	case xs of
		y : ys -> fold f (f y acc) ys
		[] -> acc
