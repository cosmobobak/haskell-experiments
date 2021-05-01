-- defining a function:
-- function :: param1Type -> param2Type -> outputType
-- function param1 param2 = output

-- haskell: [2 * x * y | x <- xs, y <- ys, x != 2, y > 10]
-- python:  [2 * x * y for x in xs for y in ys if (x != 2 and y > 10)]

-- USEFUL EXAMPLES:
identity :: Int -> Int
identity x = x

subFunctionDoubler :: Int -> Int
subFunctionDoubler x = f x
  where
    f x = x * 2

-- TASKS:
-- 1. Write a function halver :: [Int] -> [Int] to take a list of ints
--    and return the same list with each element halved.
-- a. do this with only a list comprehension
-- b. do this with the "map" higher-order function
-- c. do this with recursion

-- 2. Write a function inRange :: Int -> Int -> [Int] -> [Int] to return all
--    numbers in the input list within the range given by
--    the first two arguments (inclusive).
--        For example, inRange 5 10 [1..15] == [5,6,7,8,9,10]
-- a. do this with only a list comprehension
-- b. do this with the "filter" higher-order function
-- c. do this with recursion

-- 3.
-- a. Write a monad Container that can hold an integer.
-- b. Write a function contain :: Int -> Container that puts an
--    integer into a Container.
-- c. Write a function unbox :: Container -> Int that takes an
--    integer out of a Container.

-- 4.
-- a. Write a monad Expr that can hold
--    one or two integers.
-- b. Extend Expr to include
--    i.   Add
--    ii.  Sub
--    iii. Mult
--    iv.  Div
-- c. write a function eval :: Expr -> Int that
--    evaluates expressions.

haf :: Int -> Int
haf x = div x 2

hafcmp :: [Int] -> [Int]
hafcmp xs = [haf x | x <- xs]

hafmap :: [Int] -> [Int]
hafmap = map haf

hafrec :: [Int] -> [Int]
hafrec [x] = [haf x]
hafrec (x : xs) = haf x : hafrec xs

-----------------------------------------

inRangeCmp :: Int -> Int -> [Int] -> [Int]
inRangeCmp lo up xs = [x | x <- xs, x >= lo, x <= up]

inRangeFil :: Int -> Int -> [Int] -> [Int]
inRangeFil lo up = filter (\x -> x >= lo && x <= up)

inRangeRec :: Int -> Int -> [Int] -> [Int]
inRangeRec lo up [] = []
inRangeRec lo up (x : xs)
  | x >= lo && x <= up = x : inRangeRec lo up xs
  | otherwise = inRangeRec lo up xs

inRangeCas :: Int -> Int -> [Int] -> [Int]
inRangeCas lo up l = case l of
  [] -> []
  (x : xs)
    | x >= lo && x <= up -> x : inRangeCas lo up xs
    | otherwise -> inRangeRec lo up xs

-----------------------------------------

newtype Container = Box Int deriving Show

contain :: Int -> Container
contain = Box

unbox :: Container -> Int
unbox (Box x) = x

-----------------------------------------