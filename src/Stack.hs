module Stack where

import Data.List

newtype Stack x = ListStack [x] deriving (Show)

toStack :: [t] -> Stack t
toStack = ListStack

push :: t -> Stack t -> Stack t
push x (ListStack xs) = ListStack $ x : xs

pop :: Stack t -> Maybe (Stack t)
pop (ListStack []) = Nothing
pop (ListStack (_ : xs)) = Just $ ListStack xs

-- index starts from zero
get :: Int -> Stack t -> Maybe t
get n (ListStack xs)
  | n >= length xs = Nothing
  | otherwise = Just $ xs !! n

-- get the position of element in stack
elemIndex' :: (Eq t) => t -> Stack t -> Int
elemIndex' x (ListStack xs) = case elemIndex x xs of
  Nothing -> -1
  Just index -> index
