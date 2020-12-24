module FinalExam where

import Control.Monad
import Data.Char
import Test.QuickCheck

-- Question 1

-- 1a

f :: [String] -> String
f ss
  | length (processed ss) > 0 = head $ processed ss
  | otherwise = "zzzzz"
  where
    processed ss = [s | s <- ss, length s < 6, valid s]
    valid ss = and [isAlpha s && isLower s | s <- ss]

--Tests:
--f ["a","bb","ccc","dddd","eeeee","ffffff"] = "a" PASS
--f ["uuuuuu","vvvvv","wwww","xxx","yy","z"] = "vvvvv" PASS
--f ["Short","longer","???"] = "zzzzz" PASS
--f [] = "zzzzz" PASS
--f ["A", "A", "A", "c", "bb"] = "c" DUBIOUS - unclear whether f can assume that the input is lexicographically sorted.
--f ["A", "A", "A", "bb", "c"] = "bb" PASS

-- 1b

g :: [String] -> String
g [] = "zzzzz"
g (s : ss)
  | length s < 6 && valid s = s
  | otherwise = g ss
  where
    valid [] = True
    valid (s : ss) = isAlpha s && isLower s && valid ss

--Tests:
--g ["a","bb","ccc","dddd","eeeee","ffffff"] = "a" PASS
--g ["uuuuuu","vvvvv","wwww","xxx","yy","z"] = "vvvvv" PASS
--g ["Short","longer","???"] = "zzzzz" PASS
--g [] = "zzzzz" PASS
--g ["A", "A", "A", "c", "bb"] = "c" PASS
--g ["A", "A", "A", "bb", "c"] = "bb" PASS

-- 1c

h :: [String] -> String
h xs = case filter valid xs of
  [] -> "zzzzz"
  (x : _) -> x
  where
    valid x = length x < 6 
            && foldr (&&) True (map isAlpha x) 
            && foldr (&&) True (map isLower x)

--Tests:
--h ["a","bb","ccc","dddd","eeeee","ffffff"] = "a" PASS
--h ["uuuuuu","vvvvv","wwww","xxx","yy","z"] = "vvvvv" PASS
--h ["Short","longer","???"] = "zzzzz" PASS
--h [] = "zzzzz" PASS
--h ["A", "A", "A", "c", "bb"] = "c" PASS
--h ["A", "A", "A", "bb", "c"] = "bb" PASS

prop_fgh :: [String] -> Bool --PASS
prop_fgh ss = f ss == g ss && g ss == h ss

-- Question 2

-- 2a

i :: [a] -> [a] -> [a]
i (_ : xs) (y : _) = xs ++ [y]

-- 2b

j :: [[a]] -> [[a]]
j items = [i xs ys | (xs, ys) <- zip items shift]
  where
    shift = drop 1 items ++ [head items]

--Tests:
--j ["abc","def","ghi"] = ["bcd","efg","hia"] PASS
--j ["once","upon","a","time"] = ["nceu","pona","t","imeo"] PASS
--j ["a","b","c"] = ["b","c","a"] PASS
--j ["a"] = ["a"] PASS
--j ["abcdef", "fedcba"] = ["bcdeff","edcbaa"] PASS

-- 2c

k :: [[a]] -> [[a]]
k l@(x : _) = tracker l x
  where
    tracker :: [[a]] -> [a] -> [[a]]
    tracker is t = case is of
      [x] -> [i x t]
      (x : y : is) -> i x y : tracker (y : is) t

--Tests:
--k ["abc","def","ghi"] = ["bcd","efg","hia"] PASS
--k ["once","upon","a","time"] = ["nceu","pona","t","imeo"] PASS
--k ["a","b","c"] = ["b","c","a"] PASS
--k ["a"] = ["a"] PASS
--k ["abcdef", "fedcba"] = ["bcdeff","edcbaa"] PASS

prop_jk :: Eq a => [[a]] -> Bool --PASS
prop_jk [] = True
prop_jk [[]] = True
prop_jk xss
  | any null xss = True
  | otherwise = j xss == k xss

-- the property was written like this as 
-- empty lists are not within the specification.

-- Question 3

data Wff
  = X
  | Y
  | Tr
  | Fa
  | Not Wff
  | Wff :&: Wff
  | Wff :|: Wff
  | Wff :->: Wff
  deriving (Eq, Show)

instance Arbitrary Wff where
  arbitrary = sized gen
    where
      gen 0 =
        oneof
          [ return X,
            return Y,
            return Tr,
            return Fa
          ]
      gen n
        | n > 0 =
          oneof
            [ return X,
              return Y,
              return Tr,
              return Fa,
              liftM Not wff,
              liftM2 (:&:) wff wff,
              liftM2 (:|:) wff wff,
              liftM2 (:->:) wff wff
            ]
        where
          wff = gen (n `div` 2)

-- 3a

eval :: Bool -> Bool -> Wff -> Bool
eval x y wff = case wff of
  X -> x
  Y -> y
  Tr -> True
  Fa -> False
  Not w -> not $ eval x y w
  w1 :&: w2 -> eval x y w1 && eval x y w2
  w1 :|: w2 -> eval x y w1 || eval x y w2
  w1 :->: w2 -> eval x y (Not w1 :|: w2)

--eval False False ((X :->: Y) :&: (Not Y :|: X))  =  True PASS
--eval False True  ((X :->: Y) :&: (Not Y :|: X))  =  False PASS
--eval True  False ((X :->: Y) :&: (Not Y :|: X))  =  False PASS
--eval True  True  ((X :->: Y) :&: (Not Y :|: X))  =  True PASS

prop_and :: Bool -> Bool -> Wff -> Bool --PASS
prop_and x y wff@(X :&: Y) = eval x y wff == x && y
prop_and _ _ _ = True

prop_or :: Bool -> Bool -> Wff -> Bool --PASS
prop_or x y wff@(X :|: Y) = eval x y wff == x || y
prop_or _ _ _ = True

prop_imp :: Bool -> Bool -> Wff -> Bool --PASS
prop_imp x y wff@(X :->: Y) = eval x y wff == not x || y
prop_imp _ _ _ = True

prop_eval :: Bool -> Bool -> Wff -> Bool --PASS
prop_eval x y wff = prop_and x y wff && prop_or x y wff && prop_imp x y wff

-- 3b

simple :: Wff -> Bool
simple wff = case wff of
  Tr -> True
  Fa -> True
  Not Tr -> False
  Not Fa -> False
  Not w -> noLiterals w
  w1 :|: w2 -> noLiterals w1 && noLiterals w2
  w1 :&: w2 -> noLiterals w1 && noLiterals w2
  w1 :->: w2 -> noLiterals w1 && noLiterals w2
  _ -> True
  where
    noLiterals wff = case wff of
      Tr -> False
      Fa -> False
      Not w -> noLiterals w
      w1 :|: w2 -> noLiterals w1 && noLiterals w2
      w1 :&: w2 -> noLiterals w1 && noLiterals w2
      w1 :->: w2 -> noLiterals w1 && noLiterals w2
      _ -> True

-- simple Tr = True PASS
-- simple Fa = True PASS
-- simple ((Tr :|: X) :->: (Tr :&: Y)) = False PASS
-- simple ((X :|: Fa) :->: (Y :&: Fa)) = False PASS
-- simple ((X :&: Y) :->: (X :|: Y)) = True PASS
-- simple X = True PASS
-- simple (X :&: X :&: X :&: X :&: X :&: X) = True PASS
-- simple (Not Tr :|: Y) = False PASS
-- simple ((X :&: Not Tr) :|: Y) = False PASS

-- 3c

simplify :: Wff -> Wff
simplify wff
  | simple wff = wff
  | otherwise = simplify $ sweep wff
  where
    sweep wff = case wff of
      Not Tr -> Fa
      Not Fa -> Tr
      Fa :&: _ -> Fa
      _ :&: Fa -> Fa
      Tr :&: p -> p
      p :&: Tr -> p
      Fa :|: p -> p
      p :|: Fa -> p
      Tr :|: _ -> Tr
      _ :|: Tr -> Tr
      Fa :->: _ -> Tr
      _ :->: Tr -> Tr
      Tr :->: p -> p
      p :->: Fa -> Not p
      Not p -> Not (simplify p)
      w1 :->: w2 -> Not (simplify w1) :|: simplify w2
      w1 :&: w2 -> simplify w1 :&: simplify w2
      w1 :|: w2 -> simplify w1 :|: simplify w2
      w -> w

-- simplify Tr = Tr PASS
-- simplify Fa = Fa PASS
-- simplify ((Tr :|: X) :->: (Tr :&: Y)) = Y PASS
-- simplify ((X :|: Fa) :->: (X :&: Fa)) = Not X PASS
-- simplify ((X :|: Y) :->: (X :&: Y)) = ((X :|: Y) :->: (X :&: Y)) PASS
-- simplify (X :&: X :&: X :&: Tr) = ((X :&: X) :&: X) PASS
-- simplify (X :&: X :&: X :&: Fa) = Fa PASS