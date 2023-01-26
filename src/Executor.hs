module Executor where

import Compiler hiding (toMeta, toMetaExprs, varToStr)
import Language (CoreProgram)
import Parser (program)

-- this function gets String input and returns the result of program execution
--execute :: String -> String
--execute = toState . program

reduce :: MetaExpr -> MetaExpr
reduce (VLabel x) = Num n where 
 n = 5
  -- we need to get variable value from stack