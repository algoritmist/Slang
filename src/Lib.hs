module Lib
    ( someFunc
    ) where
import Parser
import Text.Parsec.Prim (parse)

someFunc :: IO ()
someFunc = print $ parse definition "test" "f x y z = factorial x + y + hypot x y * z;"
