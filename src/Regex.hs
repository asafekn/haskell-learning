module Regex where

import Data.List (tails)

test :: String -> String -> Bool
test regex str =
  case regex of
    '^' : rs -> testAux rs str
    _ -> or (map (testAux regex) (tails str))

testAux :: String -> String -> Bool
testAux regex str =
  case regex of
    [] -> True
    '$' : _ -> str == []

    c : '*' : rs ->
      case str of
        [] -> testAux rs str
        s : ss ->
          if s == c
          then testAux regex ss || testAux rs str || testAux rs ss
          else testAux rs str

    c : '+' : rs ->
      case str of
         [] -> False
         s : ss ->
          if s == c
          then testAux regex ss || testAux rs ss
          else False

    c : rs ->
      case str of
        [] -> False
        s : ss ->
          if c == s
          then testAux rs ss
          else False


