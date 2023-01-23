module Lib
    ( someFunc
    ) where
import Parser
import Text.Parsec.Prim (parse)

someFunc :: IO ()
someFunc = print $ parse program "test" "f x = x * x ; square x y = f x + f y;"
