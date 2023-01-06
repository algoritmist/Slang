module Parsher where

import Language
import Data.Char

clex :: String -> [Token]

syntax :: [Token] -> CoreProgram

parse :: String -> CoreProgram

parse = syntax . clex

type Token = String

clex [] = []

clex (c:cs)
  | isSeparator c = clex cs
  | isDigit c = num_token : clex rest_cs where
      num_token = c: takeWhile isDigit cs
      rest_cs = dropWhile isDigit cs
  | isAlpha c = var_token : clex rest_cs where
      var_token = c : takeWhile isAlpha cs
      rest_cs = dropWhile isAlpha cs
  | otherwise = [c] : clex cs