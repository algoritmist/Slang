module Lib
    ( someFunc
    ) where
import Parser
import Text.Parsec.Prim (parse)

someFunc :: IO ()
someFunc = print $ parse definition "test" "f x y = x*2-y"
