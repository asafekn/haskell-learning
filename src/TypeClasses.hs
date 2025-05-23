{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}

module TypeClasses where

class ToString a where
  toString :: a -> String

instance ToString Bool where
  toString :: Bool -> String
  toString bool =
    if bool
    then "True"
    else "False"

instance ToString Int where
  toString :: Int -> String
  toString number =
    case number of
        1 -> "One"
        2 -> "Two"
        3 -> "Three"
        _ -> "More than three"

listToStringList :: ToString a => [a] -> [String]
listToStringList xs = map toString xs

-- =========================

data Medal = Gold | Silver | Bronze
  deriving (Eq, Show)

instance ToString Medal where
  toString medal =
    case medal of
      Gold -> "Gold"
      Silver -> "Silver"
      Bronze -> "Bronze"

instance ToString a => ToString [a] where
  toString xs =
    case xs of
      x : xs' -> toString x <> toString xs'
      [] -> ""


