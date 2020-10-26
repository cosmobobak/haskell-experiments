

n = foldr (*) 1 . map cube . filter neg
    where
        cube x  =  x * x * x
        neg x   =  x < 0

f :: [Int] -> Int
f x = n x

f2 :: [Int] -> Int
f2 xs = product [ x*x*x | x <- xs, x < 0 ]

h :: [Int] -> Int
h [] = 1
h (x:xs) | x < 0     = x * x * x * h xs
         | otherwise = h xs