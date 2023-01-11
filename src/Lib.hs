module Lib
    ( someFunc
    ) where
import Parser

someFunc :: IO ()
someFunc = putStrLn $ show $ clex "3 == 5 - 2 \n--this is a comment\n y = x^2\n"
