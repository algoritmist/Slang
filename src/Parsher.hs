module Parsher where

import Data.Char
import Language

clex :: String -> [Token]

--syntax :: [Token] -> CoreProgram

--parse :: String -> CoreProgram

--parse = syntax . clex

type Token = String

clex [] = []
clex [c] = clex [c, ' ']
--clex [c1, c2] = clex [c1, c2, ' ']
clex (c1 : c2 : cs)
  | isSeparator c1 =
    let separator_cs = dropWhile isSeparator (c2 : cs)
     in clex separator_cs
  | isDigit c1 =
    let num_token = c1 : takeWhile isDigit (c2 : cs)
        digit_cs = dropWhile isDigit (c2 : cs)
     in num_token : clex digit_cs
  | isAlpha c1 =
    let alpha_cs = dropWhile isAlpha cs
        var_token = c1 : takeWhile isAlpha cs
     in var_token : clex alpha_cs
  | isTwoCharOp [c1, c2] = [c1, c2] : clex cs
  | otherwise = [c1] : clex (c2 : cs)

twoCharOps :: [String]
twoCharOps = ["==", "~=", ">=", "<=", "->"]

isTwoCharOp :: String -> Bool
isTwoCharOp str = str `elem` twoCharOps
