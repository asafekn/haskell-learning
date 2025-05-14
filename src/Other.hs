module Other where

myVar :: Int
myVar = 2

add1 :: Int -> Int
add1 a = a + 1

tripple :: Int -> Int
tripple a = a * 3

f x =
  if x > 10
  then add1
  else tripple

