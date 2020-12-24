import Test.QuickCheck

lengthChecker :: [String] -> [Int]
lengthChecker (x:xs) = length x : lengthChecker xs

maxlen :: [String] -> Int
maxlen (x : xs) = if a > b then a else b
  where
    a = length x
    b = maxlen xs

maxval :: [String] -> String
maxval (x : xs) = tracker xs x
  where
    tracker [] m = m
    tracker (x:xs) m
      | length x > length m = tracker xs x
      | otherwise = tracker xs m

factorial :: Int -> Int
factorial 0 = 1
factorial x = x * factorial (x - 1)

chooseA :: Int -> Int -> Int
chooseA n r = div (factorial n) (factorial r) * factorial (n - r)

prop_2n :: Int -> Bool
prop_2n n = sum [n `chooseA` r | r <- [0..n]] == 2^n

pascalIteration :: [Int] -> [Int]
pascalIteration xs = [1] ++ [(xs !! n) + (xs !! (n + 1)) | n <- [0 .. (l-2)]] ++ [1]
  where
    l = length xs

pascal :: Int -> [Int]
pascal 0 = [1]
pascal n = pascalIteration (pascal (n - 1))

limIter :: Float -> Float -> Float -> Float
limIter un n x = un * (x**(1/n))

reverseMatch :: Int -> Bool
reverseMatch n | n > 3 = (pascal n !! 3) /= (2 * pascal n !! 2)
               | otherwise = True