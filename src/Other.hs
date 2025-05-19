module Other where

data IntList
  = EmptyInt
  | NodeInt Int IntList
  deriving (Show, Eq)


data BoolList = EmptyBool | NodeBool Bool BoolList

data List a = Empty | Node a (List a)
  deriving (Show, Eq)

listLength ::
