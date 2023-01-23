module Lib
    ( someFunc
    ) where
import Parser
import Text.Parsec.Prim (parse)

someFunc :: IO ()
someFunc = print $ parse definition "test" "f x y = case x+y of option 1 -> 0, option 2 -> x;"
