module Executor where

import Compiler hiding (toMeta, toMetaExprs, varToStr)
import Language
import Parser (program)
import Stack
import Utils

-- this function gets String input and returns the result of program execution
--execute :: String -> String
--execute = toState . program

-- to reduce and expression we should be aware of global State
reduce :: MetaExpr -> State -> MetaExpr
reduce (VLabel n) st = maybe ILegal Num (get n $ stack st)
-- exprs not redused here for laziness!
reduce (FunCall fun exprs) st = execute fun exprs st
reduce (UnOp op expr) st = applyUnOp op $ reduce expr st
reduce (BinOp op expr1 expr2) st = applyBinOp op (reduce expr1 st) (reduce expr2 st)
-- any other expr wont be reduced
reduce expr _ = Just expr -- let & case not supported yet

applyUnOp :: UnaryOperation -> MetaExpr -> MetaExpr
applyUnOp Neg expr = case expr of
  (Num x) -> Num $ - x
  _ -> UnOp Neg expr -- if cant reduce dont reduce, it'll be useful for laziness

applyBinOp :: BinaryOperation -> MetaExpr -> MetaExpr -> MetaExpr
applyBinOp Sum (Num x) (Num y) = Num $ x + y
applyBinOp Sub (Num x) (Num y) = Num $ x - y
applyBinOp Mul (Num x) (Num y) = Num $ x * y
applyBinOp Div (Num x) (Num y) = Num $ x / y
applyBinOp _ _ _ = ILegal --other binops not supported yet for simplicity


type FName = String
-- we need to get variable value from stack
execute :: FName -> [MetaExpr] -> State -> MetaExpr
execute fun exprs state = do
  len <- length exprs
  addr <- elemIndex' (fun, len) $ functions state -- get the address of expr in stack
  if addr == -1 then
    --writeStats state
    return $ error $ "Function \'" ++ fun ++ "\' with " ++ show len ++ "arguments not defined"
  
  else do
    -- variable stack should be mutable!
    writeStack exprs state
    addr' <- get addr $ instructions state
    if addr' == Nothing then
      return $ error $ "Internal error: no expression for function \'" ++ fun ++ "\'"
    else
      return $ reduce (unwrap addr') state