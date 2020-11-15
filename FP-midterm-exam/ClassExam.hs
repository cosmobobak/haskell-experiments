-- Informatics 1 - Functional Programming
-- Class Test 2020

module ClassExam where
import Data.Char
import Test.QuickCheck

-- Problem 1

-- a

f :: String -> Bool -- return true if all alphas are uppercase
f "" = True
f s = and [isUpper a | a <- s, isAlpha a]

-- b

g :: String -> Bool
g "" = True
g (x : xs)
  | isAlpha x = isUpper x && g xs
  | otherwise = g xs

-- c

prop_fg :: String -> Bool
prop_fg s = f s == g s

-- Problem 2

-- a

c :: String -> Bool -- tests for doubled letters in strings
c s = or [(s !! i == s !! (i + 1)) | i <- [0 .. length s - 2]]

-- b

d :: String -> Bool
d (s1 : s2 : str)
  | s1 == s2 = True
  | otherwise = d (s2 : str)
d _ = False

-- c

prop_cd :: String -> Bool
prop_cd s = c s == d s