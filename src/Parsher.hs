module Parsher where

import Data.Char
import Language

clex :: String -> [Token]

--syntax :: [Token] -> CoreProgram

--parse :: String -> CoreProgram

--parse = syntax . clex

type Token = (Int, String)

clex cs = helper cs 0

helper :: String -> Int -> [Token]
helper [] _ = []
helper [c] line = helper [c, ' '] line
--clex [c1, c2] = clex [c1, c2, ' ']
helper (c1 : c2 : cs) line
  | c1 == '\n' = helper (c2 : cs) (line + 1)
  | isSeparator c1 =
    let separator_cs = dropWhile isSeparator (c2 : cs)
     in helper separator_cs line
  | isDigit c1 =
    let num_token = (line, c1 : takeWhile isDigit (c2 : cs))
        digit_cs = dropWhile isDigit (c2 : cs)
     in num_token : helper digit_cs line
  | isAlpha c1 =
    let alpha_cs = dropWhile isAlpha cs
        var_token = (line, c1 : takeWhile isAlpha cs)
     in var_token : helper alpha_cs line
  | isTwoCharOp [c1, c2] = (line, [c1, c2]) : helper cs line
  | otherwise = (line, [c1]) : helper (c2 : cs) line

twoCharOps :: [String]
twoCharOps = ["==", "~=", ">=", "<=", "->"]

isTwoCharOp :: String -> Bool
isTwoCharOp str = str `elem` twoCharOps
