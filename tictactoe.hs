import Data.Bits

type Node = (Int, Int, Bool, [Int])

play :: Int -> Node -> Node
play i (xs, os, t, stack) = case t of
    True -> (setBit xs i, os, not t, stack)
    False -> (xs, setBit os i, not t, stack)

unplay :: Node -> Node
unplay n@(_, _, _, []) = n
unplay (xs, os, t, (s:stack)) = case t of
  True -> (clearBit xs s, os, not t, stack)
  False -> (xs, clearBit os s, not t, stack)

rowFilled :: Int -> Bool
rowFilled halfnode = (
    (testBit halfnode 0 && testBit halfnode 3 && testBit halfnode 6) || 
    (testBit halfnode 1 && testBit halfnode 4 && testBit halfnode 7) || 
    (testBit halfnode 2 && testBit halfnode 5 && testBit halfnode 8))

colFilled :: Int -> Bool
colFilled halfnode = (
    (testBit halfnode 0 && testBit halfnode 1 && testBit halfnode 2 || 
    (testBit halfnode 3 && testBit halfnode 4 && testBit halfnode 5 || 
    (testBit halfnode 6 && testBit halfnode 7 && testBit halfnode 8))

diaFilled :: Int -> Bool
diaFilled halfnode = (
    testBit halfnode 0 && testBit halfnode 4 && testBit halfnode 8) || 
    (testBit halfnode 2 && testBit halfnode 4 && testBit halfnode 6)

xwon :: Node -> Bool
xwon (xs, _, _, _) = rowFilled xs || colFilled xs

owon :: Node -> Bool
owon (_, os, _, _) = rowFilled os || colFilled os

filled :: Node -> Bool
filled n = and [testBit (fst n) i || testBit (snd n) i | i <- [0 .. 8]]

is_game_over :: Node -> Bool
is_game_over n = owon n || xwon n || filled n

negamax :: Node -> Int -> Int
negamax n turn
  | xwon n = 1 * turn
  | owon n = -1 * turn
  | filled n = 0
  | turn == 1 = maximum [if testBit (fst n) j then (-2) else negamax gn (-1) | (gn, j) <- zip [(play i (fst n), snd n) | i <- [0..8]] [1..8]]
  | turn == (-1) = maximum [if testBit (snd n) j then (-2) else negamax gn 1 | (gn, j) <- zip [(fst n, play i (snd n)) | i <- [0..8]] [1..8]]