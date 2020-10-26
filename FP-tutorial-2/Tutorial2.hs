module Tutorial2 where

import Data.Char
import Data.List
-- import Test.QuickCheck
import Control.Monad (guard)


-- 1. halveEvens

-- List-comprehension version
halveEvens :: [Int] -> [Int]
halveEvens xs = [div n 2 | n <- xs, mod n 2 == 0]


-- This is for testing only. Do not try to understand this (yet).
halveEvensReference :: [Int] -> [Int]
halveEvensReference = (>>= \x -> guard (x `mod` 2 == 0) >>= \_ -> return $ x `div` 2)


-- -- Mutual test
prop_halveEvens :: [Int] -> Bool
prop_halveEvens xs = halveEvens xs == halveEvensReference xs


-- 2. inRange

-- List-comprehension version
inRange :: Int -> Int -> [Int] -> [Int]
inRange lo hi xs = [n | n <- xs, n >= lo && n <= hi]


-- 3. countPositives: sum up all the positive numbers in a list

-- List-comprehension version
countPositives :: [Int] -> Int
countPositives list = length [n | n <- list, n > 0]


-- 4. multDigits

-- List-comprehension version
multDigits :: String -> Int
multDigits str = product [digitToInt n | n <- str, isDigit n]

countDigits :: String -> Int
countDigits str = length [n | n <- str, isDigit n]

prop_multDigits :: String -> Bool
prop_multDigits xs = multDigits xs < 9 ^ countDigits xs


-- 5. capitalise

-- List-comprehension version
capitalise :: String -> String
capitalise (head:tail) = toUpper head : [toLower c | c <- tail]


-- 6. title

lowercase :: String -> String
lowercase xs = [toLower c | c <- xs]

-- List-comprehension version
caseHandler :: String -> String
caseHandler x = if length x > 3 then capitalise x else lowercase x

title :: [String] -> [String]
title (x:xs) = capitalise x : [caseHandler s | s <- xs]


-- 7. signs

sign :: Int -> Char
sign i = if i > 0 && i < 10 then '+' else if i == 0 then '0' else if i < 0 && i > -10 then '-' else error "int out of range"

signs :: [Int] -> String
signs xs = [sign i | i <- xs, i > -10 && i < 10]


-- 8. score

isVowel :: Char -> Bool
isVowel c = elem (toLower c) ['a', 'e', 'i', 'o', 'u']

score :: Char -> Int
score x = (if isLetter x then 1 else 0) + (if isUpper x then 1 else 0) + (if isVowel x then 1 else 0) 

totalScore :: String -> Int
totalScore xs = product [score x | x <- xs, isLetter x]

prop_totalScore_pos :: String -> Bool
prop_totalScore_pos xs = totalScore xs > 0

-- 9. pennypincher

-- List-comprehension version.
discount :: Int -> Int
discount x = round (n * 0.9)
    where
        n = fromIntegral x :: Float

pennypincher :: [Int] -> Int
pennypincher prices = sum [discount n | n <- prices, discount n < 19900]

-- -- And the test itself
prop_pennypincher :: [Int] -> Bool
prop_pennypincher xs = pennypincher xs <= sum xs

-- Optional Material

-- 10. crosswordFind

-- List-comprehension version
crosswordFind :: Char -> Int -> Int -> [String] -> [String]
crosswordFind letter pos len words = [s | s <- words, length s == len, len > 0, pos > -1, s !! pos == letter]


-- 11. search

-- List-comprehension version

search :: String -> Char -> [Int]
search str goal = [fst n | n <- xs, snd n == goal]
    where
        xs = zip [1 .. length str] str

-- Depending on the property you want to test, you might want to change the type signature
prop_search :: String -> Char -> Bool
prop_search str goal = length (search str goal) > -1


-- 12. contains

contains :: String -> String -> Bool
contains _ "" = True
contains "" _ = False
contains str substr = or [isPrefixOf substr (drop x str) | x <- [0 .. length str]]

-- Depending on the property you want to test, you might want to change the type signature
prop_contains :: String -> String -> Bool
prop_contains str1 str2
    | (length str1) < (length str2) = contains str1 str2 == False
    | otherwise = True
