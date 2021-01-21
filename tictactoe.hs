import Data.Bits

type Node = (Int, Int, Int, [Int])
type Move = Int

start :: Node
start = (0, 0, -1, [])

play :: Int -> Node -> Node
play i (xs, os, t, stack) = case t of
    (-1) -> (setBit xs i, os, -t, stack)
    1 -> (xs, setBit os i, -t, stack)

unplay :: Node -> Node
unplay n@(_, _, _, []) = n
unplay (xs, os, t, s:stack) = case t of
  (-1) -> (clearBit xs s, os, -t, stack)
  1 -> (xs, clearBit os s, -t, stack)

rowFilled :: Int -> Bool
rowFilled halfnode = 
    (testBit halfnode 0 && testBit halfnode 3 && testBit halfnode 6) || 
    (testBit halfnode 1 && testBit halfnode 4 && testBit halfnode 7) || 
    (testBit halfnode 2 && testBit halfnode 5 && testBit halfnode 8)

colFilled :: Int -> Bool
colFilled halfnode = 
    (testBit halfnode 0 && testBit halfnode 1 && testBit halfnode 2) || 
    (testBit halfnode 3 && testBit halfnode 4 && testBit halfnode 5) || 
    (testBit halfnode 6 && testBit halfnode 7 && testBit halfnode 8)

diaFilled :: Int -> Bool
diaFilled halfnode = (testBit halfnode 0 && testBit halfnode 4 && testBit halfnode 8) || (testBit halfnode 2 && testBit halfnode 4 && testBit halfnode 6)

xwon :: Node -> Bool
xwon (xs, _, _, _) = rowFilled xs || colFilled xs || diaFilled xs

owon :: Node -> Bool
owon (_, os, _, _) = rowFilled os || colFilled os || diaFilled os

pos_filled :: Node -> Int -> Bool
pos_filled (xs, os, _, _) x = testBit xs x || testBit os x

player_at :: Node -> Int -> Bool
player_at (xs, os, _, _) x = testBit xs x

filled :: Node -> Bool
filled n = and [pos_filled n i | i <- [0 .. 8]]

is_game_over :: Node -> Bool
is_game_over n = owon n || xwon n || filled n

legal_moves :: Node -> [Move]
legal_moves n = filter (not . pos_filled n) [0..8]

argmax :: [a] -> (a -> Int) -> a
argmax (x : xs) f = tracker xs x
  where
    tracker [] m = m
    tracker (x:xs) m
      | f x > f m = tracker xs x
      | otherwise = tracker xs m

negamax :: Node -> Int -> Int
negamax n@(xs, os, inturn, stack) turn
  | xwon n = 1
  | owon n = -1
  | filled n = 0
  | otherwise = maximum [-negamax childNode (-turn) | childNode <- map (\m -> play m n) (legal_moves n)]

engine_move :: Node -> Node
engine_move n = argmax (map (\m -> play m n) (legal_moves n)) (\x@(_, _, t, _) -> negamax x t)

intersperse :: a -> [a] -> [a]
intersperse s [] = [s]
intersperse s (x : xs) = s : x : intersperse s xs

showNode :: Node -> String
showNode n@(xs, os, t, stack) = concat $ intersperse "\n" blocks
  where
    blocks = groupBy 6 (intersperse ' ' [if pos_filled n x then (if player_at n x then 'X' else 'O') else '.' | x <- [0 .. 8]])
    groupBy n xs = [[xs !! (i + l) | i <- [0 .. n -1]] | l <- [0, n .. length xs - n]]