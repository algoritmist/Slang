module Lib
    ( someFunc
    ) where
import Executor

someFunc :: IO ()
someFunc = print $ runProgram "test" "K x y = y; f x = f x; main = K (f 3) 5;"

