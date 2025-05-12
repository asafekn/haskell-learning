module List where

data IntList = Empty | Node Int IntList
  deriving (Show)

twoNumbers = Node 1 (Node 2 Empty)

intHead =
