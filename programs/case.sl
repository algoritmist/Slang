f x = case x of
    1 -> 1
    2 -> 1
    otherwise -> f (-1+x) + f(-2+x);
main = f 30;