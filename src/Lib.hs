module Lib
    ( someFunc
    ) where
import Executor

someFunc :: IO ()
someFunc = print $ runProgram "test" "square x = x * x; main = square 3;"

