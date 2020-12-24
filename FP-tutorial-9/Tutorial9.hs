module Tutorial9 where

-- Sudoku solver
-- Based on Bird, "Thinking Functionally with Haskell"

import Data.List (sort,nub,(\\),transpose,genericLength)
import Data.String (lines,unlines)
import Test.QuickCheck


type Row a    = [a]
type Matrix a = [Row a]
type Digit    = Char

digits :: [Digit]
digits = ['1'..'9']

blank :: Digit -> Bool
blank d = d == ' '

-- 1.
group :: [a] -> [[a]]
group = groupBy 3

groupBy :: Int -> [a] -> [[a]]
groupBy n xs = [[xs !! (i + l) | i <- [0 .. n-1]] | l <- [0, n .. length xs - n]]
-- 2.
intersperse :: a -> [a] -> [a]
intersperse s [] = [s]
intersperse s (x:xs) = s : x : intersperse s xs

-- 3.
showRow :: String -> String
showRow = concat . intersperse "|" . group

-- 4.
showGrid :: Matrix Digit -> [String]
showGrid [] = ["-------------"]
showGrid (r1:r2:r3:matrix) = "-------------" : r1 : r2 : r3 : showGrid matrix

-- 5.
put :: Matrix Digit -> IO ()
put = putStrLn . unlines . showGrid . map showRow

-- 6.
showMat :: Matrix Digit -> String
showMat = concat . map (map cHandler)
  where 
    cHandler c = case c of
        ' ' -> '.'
        a -> a

readMat :: String -> Matrix Digit
readMat = groupBy 9 . map cHandler
  where 
    cHandler c = case c of
        '.' -> ' '
        a -> a

-- 7.
choices :: Matrix Digit -> Matrix [Digit]
choices = (map . map) digitiser
  where
    digitiser d = case d of
        ' ' -> "123456789"
        a -> [a]

-- 8.
cp :: [[a]] -> [[a]]
cp []       = [[]]
cp (xs:xss) = [ x:ys | x <- xs, ys <- cp xss ]

prop_cp :: [[a]] -> Bool
prop_cp xss = length (cp xss) == product (map length xss)
-- slow: use `quickCheckWith (stdArgs {maxSize = 5})`

expand :: Matrix [Digit] -> [Matrix Digit]
expand [] = [[]]
expand matrix = cp (map cp matrix)

-- 9.
prop_expand :: Matrix [Digit] -> Bool
prop_expand m = length (expand m) == product (concat ((map . map) length m))

-- 10.
easySols :: Integer
easySols = fromIntegral $ length $ expand $ choices easy

-- 11, 12, 13.
rows, cols, boxs :: Matrix a -> Matrix a
rows = id
cols m = case m of
    ([] : _) -> []
    m -> map head m : cols (map tail m)
boxs m = [[(m !! (y+dy)) !! (x+dx) | (y, x) <- indexzip] | (dy, dx) <- modzip]
  where
    indexzip = zip [0, 0, 0, 1, 1, 1, 2, 2, 2] [0, 1, 2, 0, 1, 2, 0, 1, 2]
    modzip = zip [0, 0, 0, 3, 3, 3, 6, 6, 6] [0, 3, 6, 0, 3, 6, 0, 3, 6]

-- 14.
distinct :: Eq a => [a] -> Bool
distinct xs = xs == nub xs

-- 15.
valid :: Matrix Digit -> Bool
valid g = all distinct (rows g) && all distinct (cols g) && all distinct (boxs g)

-- 16.
simple :: Matrix Digit -> [Matrix Digit]
simple = filter valid . expand . choices

-- 17.
the :: [Digit] -> Digit
the [d] = d

pruneRow :: Row [Digit] -> Row [Digit]
pruneRow row = map prune row
  where
    prune cs
      | length cs == 1 = cs
      | otherwise = filter (not . appearsAsSingleton) cs
    appearsAsSingleton c = [c] `elem` singletons
    singletons = [c | c <- row, length c == 1]
    
-- 18.
pruneBy :: (Matrix [Digit] -> Matrix [Digit]) -> Matrix [Digit] -> Matrix [Digit]
pruneBy f = f . map pruneRow . f

prune :: Matrix [Digit] -> Matrix [Digit]
prune = pruneBy boxs . pruneBy cols . pruneBy rows

-- 19.
many :: Eq a => (a -> a) -> a -> a
many f x
  | f x == x = x
  | otherwise = many f (f x) 

-- 20.
extract :: Matrix [Digit] -> Matrix Digit
extract = map $ map the

-- 21.
solve :: Matrix Digit -> Matrix Digit
solve m = extract (many prune (choices m))


-- ** Optional Material

-- 22.
failed :: Matrix [Digit] -> Bool
failed = any (any null)

-- 23.
solved :: Matrix [Digit] -> Bool
solved = all (all single)
  where
    single x = length x == 1

-- 24.
shortest :: Matrix [Digit] -> Int
shortest matrix = minimum [minimum [length digits | digits <- r, (not . single) digits] | r <- matrix]
  where
    single x = length x == 1

-- 25.
expand1 :: Matrix [Digit] -> [Matrix [Digit]]
expand1 mat = undefined 

-- 26.
search :: Matrix Digit -> [Matrix Digit]
search = undefined


-- Example from Bird

book :: Matrix Digit
book = ["  4  57  ",
        "     94  ",
        "36      8",
        "72  6    ",
        "   4 2   ",
        "    8  93",
        "4      56",
        "  53     ",
        "  61  9  "]

-- Examples from websudoku.com

easy :: Matrix Digit
easy = ["    345  ",
        "  89   3 ",
        "3    2789",
        "2 4  6815",
        "    4    ",
        "8765  4 2",
        "7523    6",
        " 1   79  ",
        "  942    "]

medium :: Matrix Digit
medium = ["   4 6 9 ",
          "     3  5",
          "45     86",
          "6 2 74  1",
          "    9    ",
          "9  56 7 8",
          "71     64",
          "3  6     ",
          " 6 9 2   "]

hard :: Matrix Digit
hard = ["9 3  42  ",
        "4 65     ",
        "  28     ",
        "     5  4",
        " 67 4 92 ",
        "1  9     ",
        "     87  ",
        "     94 3",
        "  83  6 1"]

evil :: Matrix Digit
evil = ["  9      ",
        "384   5  ",
        "    4 3  ",
        "   1  27 ",
        "2  3 4  5",
        " 48  6   ",
        "  6 1    ",
        "  7   629",
        "     5   "]

puts :: [Matrix Digit] -> IO ()
puts = sequence_ . map put

puzzle :: Matrix Digit -> IO ()
puzzle g = put g >> puts (search g) >> putStrLn "***"
       
main :: IO ()
main = puzzle easy >>
       puzzle medium >>
       puzzle hard >>
       puzzle evil

