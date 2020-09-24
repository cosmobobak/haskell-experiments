square :: Integer -> Integer
square x = x * x

square_add :: Integer -> Integer -> Integer
square_add a b = square a + square b

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = (fib (n-1) + fib (n-2))

