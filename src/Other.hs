module Other where

data IntList
  = EmptyInt
  | NodeInt Int IntList
  deriving (Show, Eq)


data BoolList = EmptyBool | NodeBool Bool BoolList

data List a = Empty | Node a (List a)
  deriving (Show, Eq)

flatMap :: (a -> [b]) -> [a] -> [b]
flatMap f xs =
    case xs of
      x : xs' -> f x <> flatMap f xs'
      [] -> []

a0 :: [Int]
a0 =
  flatMap (\x -> [x + 1, x + 2, x + 3])    -- [4,5,6,2,3,4,4,5,6,2,3,4,7,8,9,3,4,5,7,8,9,3,4,5]
  (flatMap (\x -> [x * 3, x])              -- [3,1,3,1,6,2,6,2]
  (flatMap (\x -> [x, x]) [1,2])           -- [1,1,2,2]
  )

a1 :: [Int]
a1 =
  [1,2] >>=
  (\x -> [x, x]) >>=
  (\x -> [x * 3, x]) >>=
  (\x -> [x + 1, x + 2, x + 3])


a2 :: [Int]
a2 =
  [1,2] >>= (\x ->
  [x, x]) >>= (\x ->
  [x * 3, x]) >>= (\x ->
  [x + 1, x + 2, x + 3])


a3 :: [Int]
a3 =
  [1,2] >>= (\x ->
  [x, x]) >>= (\y ->
  [y * 3, y]) >>= (\z ->
  [z + 1, z + 2, z + 3])

a4 :: [Int]
a4 = do
  x <- [1,2]
  y <- [x, x]
  z <- [y * 3, y]
  w <- [z + 1, z + 2, z + 3]
  return w

a5 :: [Int]
a5 = do
  x <- [1,2]
  y <- [x, x]
  z <- [y * 3, y]
  [z + 1, z + 2, z + 3]

hello :: Int -> [String]
hello n = do
  x <- [n, n + 1]
  return ("Hello " <> show x)

