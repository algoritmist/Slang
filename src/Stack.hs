module Stack where

newtype Stack x = ListStack [x] deriving (Show)

push :: t -> Stack t -> Stack t
push x (ListStack xs) = ListStack $ x : xs

pop :: Stack t -> Maybe (Stack t)
pop (ListStack []) = Nothing
pop (ListStack (_ : xs)) = Just $ ListStack xs

-- index starts from zero --
get :: Int -> Stack t -> Maybe t
get n (ListStack xs)
  | n >= length xs = Nothing
  | otherwise = Just $ xs !! n
