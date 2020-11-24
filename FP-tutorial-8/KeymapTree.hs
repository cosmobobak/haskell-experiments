-- Indexed data represented as a tree

module KeymapTree ( Keymap,
                    size, depth,
                    get, set, del,
                    select,
                    toList, fromList,
                    merge, filterLT, filterGT,
                    prop_set_get, prop_toList_fromList
                  )

where

-- Modules for testing

import Test.QuickCheck
import Control.Monad
import Data.List

-- The data type

data Keymap k a = Leaf
                | Node k a (Keymap k a) (Keymap k a)

-- A test tree

testTree :: Keymap Int Int
testTree = Node 2 20 (Node 1 10 Leaf Leaf)
                     (Node 3 30 Leaf 
                               (Node 4 40 Leaf Leaf ))
-- Exercise 6

size :: Ord k => Keymap k a -> Int
size Leaf = 0
size (Node _ _ left right) = 1 + size left + size right

-- size first patternmatches against a leaf node - essentially the base case for treesearch
-- if Leaf is not matched, we are at either the root or a branch, and so we recurse down, adding one for the current node and summing the two forward paths.

depth :: Ord k => Keymap k a -> Int
depth Leaf = 0
depth (Node _ _ left right) = 1 + max (depth left) (depth right)

-- Exercise 7

toList :: Ord k => Keymap k a -> [(k, a)]
toList Leaf = []
toList (Node k v left right) = sortBy (\(k, _) (k2, _) -> if k > k2 then GT else LT) (toList left ++ [(k, v)] ++ toList right)

-- Exercise 8

set :: Ord k => k -> a -> Keymap k a -> Keymap k a
set key value = go
  where
    go Leaf = Node key value Leaf Leaf
    go (Node k v left right)
      | key == k = Node k value left right
      | key < k = Node k v (go left) right
      | key > k = Node k v left (go right)

-- (b) Explain what the function go does when it encounters a leaf.
-- go replaces the leaf node with a content-containing branch node.

-- (c) Explain what the function go does when it looks at a node and it encounters the key it was looking for? Complete the definition of this function.  Hint:  the last two cases will need to recurse down an appropriate branch.6
-- if go encounters a matched key, it replaces the entry with the content it was passed.

-- Exercise 9

get :: Ord k => k -> Keymap k a -> Maybe a
get key = search
  where
    search Leaf = Nothing
    search (Node k val left right)
      | key == k = Just val
      | key < k = search left
      | key > k = search right

prop_set_get :: Int -> Int -> Bool
prop_set_get k v = get k (set k v testTree) == Just v

-- Exercise 10

fromList :: Ord k => [(k, a)] -> Keymap k a
fromList xs = treeify (sortBy (\(k, _) (k2, _) -> if k > k2 then GT else LT) xs)
  where
    treeify [] = Leaf
    treeify xs = Node (mk xs) (mv xs) (treeify (left xs)) (treeify (right xs))
    mk xs = fst (middle xs)
    mv xs = snd (middle xs)
    middle xs = xs !! (length xs `div` 2)
    left xs = take (length xs `div` 2) xs
    right xs = drop ((length xs `div` 2) + 1) xs

prop_toList_fromList :: [Int] -> [Int] -> Bool
prop_toList_fromList xs ys = sort (toList (fromList zs)) == sort zs
  where
    zs = zip (nub xs) ys

prop_toList_fromList_sorted :: [Int] -> [Int] -> Bool
prop_toList_fromList_sorted xs ys = toList (fromList zs) == sort zs
  where
    zs = zip (nub xs) ys

-- Optional Material -----------------------------------

-- Exercise 12

filterLT :: Ord k => k -> Keymap k a -> Keymap k a
filterLT = undefined

filterGT :: Ord k => k -> Keymap k a -> Keymap k a
filterGT = undefined

-- Exercise 13

merge :: Ord k => Keymap k a -> Keymap k a -> Keymap k a
merge = undefined

-- Exercise 14

del :: Ord k => k -> Keymap k a -> Keymap k a
del = undefined

-- Exercise 15

select :: Ord k => (a -> Bool) -> Keymap k a -> Keymap k a
select = undefined

-- Instances for QuickCheck -----------------------------
instance (Ord k, Show k, Show a) => Show (Keymap k a) where
  show = show . toList

instance (Ord k, Arbitrary k, Arbitrary a) => Arbitrary (Keymap k a) where
  arbitrary = liftM fromList $ liftM2 zip (liftM nub arbitrary) arbitrary
