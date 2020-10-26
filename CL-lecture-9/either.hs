-- this is already built in
-- data Either a b = Left a | Right b

mylist :: [Either Int String]
mylist  =  [Left 4, Left 1, Right "hello", Left 2,
            Right " ", Right "world", Left 17]

addints :: [Either Int String] -> Int
addints []              =  0
addints (Left n : xs)   =  n + addints xs
addints (Right s : xs)  =  addints xs

addints' :: [Either Int String] -> Int
addints' xs  =  sum [n | Left n <- xs]

addstrs :: [Either Int String] -> String
addstrs []              =  ""
addstrs (Left n : xs)   =  addstrs xs
addstrs (Right s : xs)  =  s ++ addstrs xs

addstrs' :: [Either Int String] -> String
addstrs' xs  =  concat [s | Right s <- xs]

