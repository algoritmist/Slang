module Utils where


fstMap :: [(a, b)] -> [a]
fstMap = fmap fst

sndMap :: [(a, b)] -> [b]
sndMap = fmap snd


unwrap :: Maybe t -> t
unwrap (Just x) = x
unwrap Nothing = error "cant unwrap empty value"