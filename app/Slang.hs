module Slang (main) where

import Executor
import Parser
import System.Environment.Blank (getArgs)
import Text.Parsec.Prim (parse)

main :: IO ()
main = do
  args <- getArgs
  if length args /= 1
    then do
      putStrLn "usage: slang <source_file.sl>"
    else
      let file = head args
       in do
            content <- readFile file
            --print $ parse program file content
            putStrLn $ "main = " ++ runProgram file content
