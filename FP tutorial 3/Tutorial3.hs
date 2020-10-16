module Tutorial3 where

import Data.Char
import Data.List
--import Test.QuickCheck

-- 1.

halveEvensRec :: [Int] -> [Int]
halveEvensRec [] = []
halveEvensRec (x : xs)
  | x `mod` 2 == 0 = x `div` 2 : halveEvensRec xs
  | otherwise = halveEvensRec xs

halveEvens :: [Int] -> [Int]
halveEvens xs = [x `div` 2 | x <- xs, x `mod` 2 == 0]

prop_halveEvens :: [Int] -> Bool
prop_halveEvens xs = halveEvensRec xs == halveEvens xs

-- 2.

inRangeRec :: Int -> Int -> [Int] -> [Int]
inRangeRec _ _ [] = []
inRangeRec lo hi (x : xs)
  | x >= lo, x <= hi = x : inRangeRec lo hi xs
  | otherwise = inRangeRec lo hi xs

inRange :: Int -> Int -> [Int] -> [Int]
inRange lo hi xs = [x | x <- xs, lo <= x, x <= hi]

prop_inRange :: Int -> Int -> [Int] -> Bool
prop_inRange lo hi xs = inRangeRec lo hi xs == inRange lo hi xs

-- 3.

countPositivesRec :: [Int] -> Int
countPositivesRec [] = 0
countPositivesRec (x : xs)
  | x > 0 = 1 + countPositivesRec xs
  | otherwise = countPositivesRec xs

countPositives :: [Int] -> Int
countPositives list = length [x | x <- list, x > 0]

prop_countPositives :: [Int] -> Bool
prop_countPositives l = countPositivesRec l == countPositives l

-- 4.

multDigitsRec :: String -> Int
multDigitsRec [] = 1
multDigitsRec (x : xs)
  | isDigit x = digitToInt x * multDigitsRec xs
  | otherwise = multDigitsRec xs

multDigits :: String -> Int
multDigits str = product [digitToInt ch | ch <- str, isDigit ch]

prop_multDigits :: String -> Bool
prop_multDigits xs = multDigitsRec xs == multDigits xs

-- These are some helper functions for makeKey and makeKey itself.
-- Exercises continue below.

rotate :: Int -> [Char] -> [Char]
rotate k list
  | 0 <= k && k <= length list = drop k list ++ take k list
  | otherwise = error "Argument to rotate too large or too small"

--  prop_rotate rotates a list of lenght l first an arbitrary number m times,
--  and then rotates it l-m times; together (m + l - m = l) it rotates it all
--  the way round, back to the original list
--
--  to avoid errors with 'rotate', m should be between 0 and l; to get m
--  from a random number k we use k `mod` l (but then l can't be 0,
--  since you can't divide by 0)
prop_rotate :: Int -> String -> Bool
prop_rotate k str = rotate (l - m) (rotate m str) == str
  where
    l = length str
    m = if l == 0 then 0 else k `mod` l

alphabet = ['A' .. 'Z']

makeKey :: Int -> [(Char, Char)]
makeKey k = zip alphabet (rotate k alphabet)

-- Ceasar Cipher Exercises
-- =======================

-- 5.

lookUp :: Char -> [(Char, Char)] -> Char
lookUp ch xs = if length match > 0 then snd (match !! 0) else ch
  where
    match = [cpair | cpair <- xs, fst cpair == ch]

lookUpRec :: Char -> [(Char, Char)] -> Char
lookUpRec ch [] = ch
lookUpRec ch (x : xs)
  | fst x == ch = snd x
  | otherwise = lookUpRec ch xs

prop_lookUp :: Char -> [(Char, Char)] -> Bool
prop_lookUp c k = lookUp c k == lookUpRec c k

-- 6.

encipher :: Int -> Char -> Char
encipher k ch = lookUp ch (makeKey k)

-- 7.

normalize :: String -> String
normalize [] = []
normalize xs = [toUpper c | c <- xs, isAlphaNum c]

encipherStr :: Int -> String -> String
encipherStr k str = [encipher k c | c <- normalize str]

-- 8.

reverseKey :: [(Char, Char)] -> [(Char, Char)]
reverseKey xs = [(snd c, fst c) | c <- xs]

reverseKeyRec :: [(Char, Char)] -> [(Char, Char)]
reverseKeyRec [] = []
reverseKeyRec (x : xs) = (snd x, fst x) : reverseKeyRec xs

prop_reverseKey :: [(Char, Char)] -> Bool
prop_reverseKey xs = reverseKey xs == reverseKeyRec xs

-- 9.

decipher :: Int -> Char -> Char
decipher k ch = lookUp ch (reverseKey (makeKey k))

decipherStr :: Int -> String -> String
decipherStr k str = [decipher k c | c <- str]

-- Optional Material
-- =================

-- 10.

contains :: String -> String -> Bool
contains _ "" = True
contains "" _ = False
contains str substr = or [isPrefixOf substr (drop x str) | x <- [0 .. length str]]

-- 11.

candidates :: String -> [(Int, String)]
candidates str = [(i, attempt i) | i <- [0 .. length alphabet], valid i]
  where
    attempt i = decipherStr i str
    valid i = contains (attempt i) "AND" || contains (attempt i) "THE"

-- 12.

splitEachFive :: String -> [String]
splitEachFive "" = []
splitEachFive x
  | length x > 4 = (take 5 x) : splitEachFive (drop 5 x)
  | otherwise = [x ++ ['X' | _ <- [1 .. invlen x]]]
  where
    invlen x = 5 - length x

prop_transpose :: String -> Bool
prop_transpose s = transpose (transpose (splitEachFive s)) == splitEachFive s

--transpose (transpose ([[1,2,3],[9,9,9,9,9,9],[1,1]])) is not equal to [[1,2,3],[9,9,9,9,9,9],[1,1]]

-- 13.
encrypt :: Int -> String -> String
encrypt n s = concat (transpose (splitEachFive (encipherStr n s)))

-- 14.
smartSplit :: Int -> String -> [String]
smartSplit _ "" = []
smartSplit n x
  | length x > n - 1 = (take n x) : smartSplit n (drop n x)
  | length x > 0 = [x ++ ['X' | _ <- [1 .. invlen x]]]
  where
    invlen x = n - length x

decrypt :: Int -> String -> String
decrypt n s = decipherStr n (concat (transpose (smartSplit (length s `div` 5) s)))
