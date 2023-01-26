module Executor where

import Compiler hiding (toMeta, toMetaExprs, varToStr)
import Language (CoreProgram)
import Parser (program)
import Stack

-- this function gets String input and returns the result of program execution
--execute :: String -> String
--execute = toState . program

-- to reduce and expression we should be aware of global State
reduce :: MetaExpr -> State -> Maybe MetaExpr
reduce (VLabel n) st = case get n $ stack st of
      

-- we need to get variable value from stack
