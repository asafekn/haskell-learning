module Main where

main :: IO ()
main = putStrLn "Hello, Haskell!"

multiply a b = a * b

isEven a = 
        if (mod a 2 == 0) 
        then "Even" 
        else ("Odd") 

repeatNTimes acc n = 
        if (acc < n) 
        then "h" <> repeatNTimes (acc + 1) n
        else ""
