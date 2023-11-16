module Slang (main) where

--import Executor
import Parser
import GFC
import System.Environment.Blank (getArgs)
import Text.Parsec.Prim (parse)




main :: IO ()
main = do
  args <- getArgs
  if length args /= 1
    then do
      putStrLn "usage: Slang <source_file.sl>"
    else
      let file = head args
       in do
            content <- readFile file
            let prog = parse program file content
            print prog
            let flowControlGraph = either (error.show) genGraphs prog
            print flowControlGraph
            --putStrLn $ "main = " ++ runProgram file content
