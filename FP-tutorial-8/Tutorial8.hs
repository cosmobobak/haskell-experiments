module Tutorial8 where

import System.Random

-- Importing the keymap module

import KeymapList


-- Type declarations

type Barcode = String
type Product = String
type Unit    = String

type Item    = (Product,Unit)

type Catalogue = Keymap Barcode Item


-- A little test catalog

testDB :: Catalogue
testDB = fromList [
 ("0265090316581", ("The Macannihav'nmor Highland Single Malt", "75ml bottle")),
 ("0903900739533", ("Bagpipes of Glory", "6-CD Box")),
 ("9780201342758", ("Thompson - \"Haskell: The Craft of Functional Programming\"", "Book")),
 ("0042400212509", ("Universal deep-frying pan", "pc"))
 ]


-- Exercise 1

longestProductLen :: [(Barcode, Item)] -> Int
longestProductLen = foldl maxBarcode 0
  where
    maxBarcode x (_, (y, _)) = max x (length y)

formatLine :: Int -> (Barcode, Item) -> String
formatLine len (code, (prod, unit)) = code ++ ['.' | _ <- [1 .. 3]] ++ prod ++ ['.' | _ <- [1 .. (len - length prod)]] ++ ['.' | _ <- [1 .. 3]] ++ unit

showCatalogue :: Catalogue -> String
showCatalogue xs = concat [formatLine (longestProductLen (toList xs)) x ++ "\n" | x <- toList xs]
     
-- Exercise 2
maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just x) = [x]

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (x:_) = Just x

catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes (x:xs) = case x of
    Nothing -> catMaybes xs
    Just a -> a : catMaybes xs 

-- Exercise 3

getItems :: [Barcode] -> Catalogue -> [Item]
getItems keys d = catMaybes [get k d | k <- keys]


-- Exercise 4

-- a. 0.91 seconds
-- b. 1.04 seconds for 100 entries using [get k theDB | k <- keys] -> 0.01 seconds per lookup
-- c. 104650 items, and a database twice the size would take twice the time to index on average
 
-- For Exercises 6-10 check KeymapTree.hs 

-- Exercise 11

-- a. 3.51 seconds
-- b. 114.30 seconds for 100 entries using [get k theDB | k <- keys] -> 1.143 seconds per lookup
-- c. 17 items, as the database has depth 17.

-- ** Input-output

readDB :: IO Catalogue
readDB = do dbl <- readFile "database.csv"
            let db = fromList (map readLine $ lines dbl)
            putStrLn (size db >= 0 `seq` "Done")
            return db

readLine :: String -> (Barcode,Item)
readLine str = (a,(c,b))
    where
      (a,str2) = splitUpon ',' str
      (b,c)    = splitUpon ',' str2

splitUpon :: Char -> String -> (String,String)
splitUpon _ "" = ("","")
splitUpon c (x:xs) | x == c    = ("",xs)
                   | otherwise = (x:ys,zs)
                   where
                     (ys,zs) = splitUpon c xs

getSample :: Catalogue -> IO Barcode
getSample db = do g <- newStdGen
                  return $ fst $ toList db !! fst (randomR (0,size db - 1) g)

samples :: Int -> Catalogue -> IO [Barcode]
samples n db = sequence [getSample db | _ <- [1..n]]

gets :: [Barcode] -> Catalogue -> [Item]
gets ks db  =  [x | k <- ks, Just x <- [get k db]]
