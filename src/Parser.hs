module Parser where

import Data.Char
import Language
import Text.Regex.TDFA

clex :: String -> [Token]

--syntax :: [Token] -> CoreProgram

--parse :: String -> CoreProgram

--parse = syntax . clex

type Token = (Int, String)

clex s = helper 0 $ filterComments s

helper :: Int -> String -> [Token]
helper _ [] = []
helper line [c] = helper line [c, ' ']
--clex [c1, c2] = clex [c1, c2, ' ']
helper line (c1 : c2 : cs)
  | c1 == '\n' = helper (line + 1) (c2 : cs)
  | isSeparator c1 =
    let separator_cs = dropWhile isSeparator (c2 : cs)
     in helper line separator_cs
  | isDigit c1 =
    let num_token = (line, c1 : takeWhile isDigit (c2 : cs))
        digit_cs = dropWhile isDigit (c2 : cs)
     in num_token : helper line digit_cs
  | isAlpha c1 =
    let alpha_cs = dropWhile isAlpha cs
        var_token = (line, c1 : takeWhile isAlpha cs)
     in var_token : helper line alpha_cs
  | isTwoCharOp [c1, c2] = (line, [c1, c2]) : helper line cs
  | otherwise = (line, [c1]) : helper line (c2 : cs)

filterComments :: String -> String
filterComments = unlines . filter (\x -> not $ x =~ "(--.*)" :: Bool) . lines

twoCharOps :: [String]
twoCharOps = ["==", "~=", ">=", "<=", "->"]

isTwoCharOp :: String -> Bool
isTwoCharOp str = str `elem` twoCharOps
