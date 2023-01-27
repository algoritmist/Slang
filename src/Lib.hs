module Lib
    ( someFunc
    ) where
import Executor

someFunc :: IO ()
someFunc = print $ runProgram "test" "K x y = y; square x = x + square x; main = K (square 3) 5;"

