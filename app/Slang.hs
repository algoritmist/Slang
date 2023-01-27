module Slang (main) where

import Executor
import System.Environment.Blank (getArgs)

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
            putStrLn $ "main = " ++ runProgram file content
