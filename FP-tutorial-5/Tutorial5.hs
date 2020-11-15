module Tutorial5 where

import Data.Char
import Data.List
import Test.QuickCheck

-- 1. Map
-- a.
doubles :: [Int] -> [Int]
doubles xs = map f xs
  where
    f x = x * 2

-- b.
penceToPounds :: [Int] -> [Float]
penceToPounds xs = map f xs
  where
    f x = (fromIntegral x) / 100

-- c.
uppers :: String -> String
uppers s = map toUpper s

-- d.
uppersComp :: String -> String
uppersComp s = [toUpper c | c <- s]

prop_uppers :: String -> Bool
prop_uppers s = uppers s == uppersComp s

-- 2. Filter
-- a.
alphas :: String -> String
alphas s = filter isAlpha s

-- b.
above :: Int -> [Int] -> [Int]
above l xs = filter (f l) xs
  where
    f l x = x > l

-- c.
unequals :: [(Int, Int)] -> [(Int, Int)]
unequals xs = filter f xs
  where
    f (a, b) = a /= b

-- d.
rmChar :: Char -> String -> String
rmChar c s = filter (f c) s
  where
    f c x = c /= x

-- e.
rmCharComp :: Char -> String -> String
rmCharComp c s = [x | x <- s, c /= x]

prop_rmChar :: Char -> String -> Bool
prop_rmChar c s = rmChar c s == rmCharComp c s

-- 3. Comprehensions vs. map & filter
-- a.
largeDoubles :: [Int] -> [Int]
largeDoubles xs = [2 * x | x <- xs, x > 3]

largeDoubles' :: [Int] -> [Int]
largeDoubles' xs = map f (filter p xs)
  where
    f x = 2 * x
    p x = x > 3

prop_largeDoubles :: [Int] -> Bool
prop_largeDoubles xs = largeDoubles xs == largeDoubles' xs

-- b.
reverseEven :: [String] -> [String]
reverseEven strs = [reverse s | s <- strs, even (length s)]

reverseEven' :: [String] -> [String]
reverseEven' strs = map f (filter p strs)
  where
    f s = reverse s
    p s = even (length s)

-- 4. Foldr
-- a.
andRec :: [Bool] -> Bool
andRec [] = True
andRec (x:xs) = x && (andRec xs)

andFold :: [Bool] -> Bool
andFold xs = foldr (&&) True xs

prop_and :: [Bool] -> Bool
prop_and xs = andRec xs == andFold xs

-- b.
concatRec :: [[a]] -> [a]
concatRec [] = []
concatRec (x:xs) = x ++ concatRec xs

concatFold :: [[a]] -> [a]
concatFold xs = foldr (++) [] xs

-- c.
rmCharsRec :: String -> String -> String
rmCharsRec [] s = s
rmCharsRec (x:xs) s = rmCharsRec xs (rmChar x s)

rmCharsFold :: String -> String -> String
rmCharsFold rems string = foldr rmChar string rems

prop_rmChars :: String -> String -> Bool
prop_rmChars rems s = (rmCharsRec rems s) == (rmCharsRec rems s)

-- Matrix multiplication

type Matrix = [[Rational]]

-- 5
-- a.
uniform :: [Int] -> Bool
uniform xs = and [first - x == 0 | x <- xs]
  where
    first = xs !! 0

-- b.
valid :: Matrix -> Bool
valid m
  | length m == 0 = False
  | length (m !! 0) == 0 = False
  | otherwise = uniform (map (length) m)

-- 6.
matrixWidth :: Matrix -> Int
matrixWidth m = length (m !! 0)

matrixHeight :: Matrix -> Int
matrixHeight m = length m

plusM :: Matrix -> Matrix -> Matrix
plusM m n
  | valid m && valid n && (matrixHeight m == matrixHeight n) && (matrixWidth m == matrixWidth n) = zipWith (zipWith (+)) m n
  | otherwise = error "bad input"

-- 7.
-- height m by width n

cols :: Matrix -> Matrix
cols m = [[row !! col | row <- m] | col <- [0..c]]
  where
    c = matrixWidth m - 1

timesM :: Matrix -> Matrix -> Matrix
timesM m n
  | valid m && valid n = [[sum (zipWith (*) r c) | c <- cols n] | r <- m]
  | otherwise = error "bad input"

-- 8.
-- b.
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' f xs ys = [f (fst x) (snd x) | x <- zip xs ys]

-- c.
zipWith'' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith'' f xs ys = map (uncurry f) (zip xs ys)

-- ** Optional material

-- 9.

-- Mapping functions
mapMatrix :: (a -> b) -> [[a]] -> [[b]]
mapMatrix f = undefined

zipMatrix :: (a -> b -> c) -> [[a]] -> [[b]] -> [[c]]
zipMatrix f = undefined

-- All ways of deleting a single element from a list
removes :: [a] -> [[a]]
removes = undefined

-- Produce a matrix of minors from a given matrix
minors :: Matrix -> [[Matrix]]
minors m = undefined

-- A matrix where element a_ij = (-1)^(i + j)
signMatrix :: Int -> Int -> Matrix
signMatrix w h = undefined

determinant :: Matrix -> Rational
determinant = undefined

cofactors :: Matrix -> Matrix
cofactors m = undefined

scaleMatrix :: Rational -> Matrix -> Matrix
scaleMatrix k = undefined

inverse :: Matrix -> Matrix
inverse m = undefined

-- Tests
identity :: Int -> Matrix
identity n = undefined

prop_inverse2 ::
  Rational ->
  Rational ->
  Rational ->
  Rational ->
  Property
prop_inverse2 a b c d = undefined

type Triple a = (a, a, a)

prop_inverse3 ::
  Triple Rational ->
  Triple Rational ->
  Triple Rational ->
  Property
prop_inverse3 r1 r2 r3 = undefined
