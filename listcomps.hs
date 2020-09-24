nums :: [Integer]
nums = [42, 12, 35, 7, 5, 100]

count_up_list :: Integer -> [Integer]
count_up_list n = [1 .. n]

factorial :: Integer -> Integer
factorial n = product (count_up_list n)

square :: Integer -> Integer
square x = x * x

square_list :: Integer -> [Integer]
square_list n = [square x | x <- count_up_list n]

square_odd_list :: Integer -> [Integer]
square_odd_list n = [square x | x <- count_up_list n, odd x]