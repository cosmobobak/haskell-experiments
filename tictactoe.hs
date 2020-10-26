import Data.Bits

type Node = (Int, Int)

set_bit :: Int -> Int -> Int
set_bit i node = ((bit 1) `shiftL` i) .|. node

unset_bit :: Int -> Int -> Int
unset_bit i node = (complement ((bit 1) `shiftL` i)) .&. node

play :: Int -> Int -> Int
play square board = set_bit square board

unplay :: Int -> Int -> Int
unplay square board = unset_bit square board

row_filled :: Int -> Bool
row_filled halfnode = (
    (testBit halfnode 0 && testBit halfnode 3 && testBit halfnode 6) || 
    (testBit halfnode 1 && testBit halfnode 4 && testBit halfnode 7) || 
    (testBit halfnode 2 && testBit halfnode 5 && testBit halfnode 8))

col_filled :: Int -> Bool
col_filled halfnode = (
    (testBit halfnode 0 && testBit halfnode 1 && testBit halfnode 2) || 
    (testBit halfnode 3 && testBit halfnode 4 && testBit halfnode 5) || 
    (testBit halfnode 6 && testBit halfnode 7 && testBit halfnode 8))

xwon :: Node -> Bool
xwon n = row_filled a ||  col_filled a
  where
    a = fst n

owon :: Node -> Bool
owon n = row_filled b || col_filled b
  where
    b = snd n

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