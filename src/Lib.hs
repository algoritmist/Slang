module Lib
    ( someFunc
    ) where
import Parser
import Text.Parsec.Prim (parse)
import Compiler

someFunc :: IO ()
someFunc = print $ case parse program "test" "f x y = x; g x y = f x 5 + y;" of
  (Left x) -> error $ show x
  (Right expr) -> toMetaExprs expr

