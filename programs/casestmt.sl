fib n = case n of
    1 -> 1
    2 -> 1
    otherwise -> fib (n + (-1)) + fib (n + (-2)); -- "-" is an unary operator
main = fib 30;