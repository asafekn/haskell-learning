module List where

data IntList = Empty | Node Int IntList
  deriving (Show, Eq)

data MaybeInt = NoInt | JustInt Int
  deriving (Show, Eq)

twoNumbers :: IntList
twoNumbers = Node 1 (Node 2 Empty)

anotherTwoNumbers :: IntList
anotherTwoNumbers = Node 3 (Node 4 Empty)

intHead :: IntList -> MaybeInt
intHead list =
	case list of
		Node n xs -> JustInt n
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
		Node a b -> (1 + lengthIntList b)
		Empty -> 0

-- Get the last element of the list
--
--    lastIntList (Node 1 (Node 2 Empty)) == JustInt 2
--
lastIntList :: IntList -> MaybeInt
lastIntList list =
	case list of
		Node a b -> case b of
			Node c d -> lastIntList b
			Empty -> JustInt a

		Empty -> NoInt

-- Apply function f to every element of the list
--
--     mapIntList tripple (Node 1 (Node 2 Empty)) == Node 3 (Node 6 Empty)
--
mapIntList :: (Int -> Int) -> IntList -> IntList
mapIntList f xs =
	case xs of
		Node a b -> Node(f a) (mapIntList f b)
		Empty -> Empty

-- Merge two IntLists
--
--  concatIntList (Node 1 (Node 2 Empty)) (Node 3 (Node 4 Empty)) ==
--      Node 1 (Node 2 (Node 3 (Node 4 Empty)))
--
concatIntList :: IntList -> IntList -> IntList
concatIntList xs ys =
	case xs of
		Node a b -> Node a (concatIntList b ys)
		Empty -> ys

-- Return the reversed list
--
--  reverseIntList (Node 1 (Node 2 Empty)) == Node 2 (Node 1 Empty)
--
-- reverseIntList :: IntList -> IntList
-- reverseIntList xs =
