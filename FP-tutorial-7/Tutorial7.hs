module Tutorial7 where

import LSystem (Angle, Command (Go, Sit, Turn, (:#:)), Distance)
import Test.QuickCheck ()

-- import Text.PrettyPrint.GenericPretty -- ** Uncomment for Generic Pretty Challenge

pathExample :: Command
pathExample = Go 30 :#: Turn 120 :#: Go 30 :#: Turn 120 :#: Go 30

-- 1a. split
split :: Command -> [Command]
split Sit = []
split (Go a) = [Go a]
split (Turn a) = [Turn a]
split (x :#: y) = split x ++ split y

-- 1b. join
join :: [Command] -> Command
join = foldr (:#:) Sit

-- 1c. equivalent
-- equivalent :: ???
equivalent :: Command -> Command -> Bool
equivalent comm1 comm2 = split comm1 == split comm2

-- 1d. testing join and split
prop_split_join :: Command -> Bool
prop_split_join c = equivalent c (join (split c))

prop_split :: Command -> Bool
prop_split ct =
  and
    [ case c of
        (Go _) -> True
        (Turn _) -> True
        _ -> False
      | c <- split ct
    ]

-- 2a. copy
copy :: Int -> Command -> Command
copy n c
  | n < 1 = Sit
  | otherwise = join [c | _ <- [1 .. n]]

-- 2b. pentagon
pentagon :: Distance -> Command
pentagon d = copy 5 (Go d :#: Turn 72)

-- 2c. polygon
polygon :: Distance -> Int -> Command
polygon d n = copy n (Go d :#: Turn (fromIntegral r))
  where
    r = 360 `div` n

-- 3. spiral
spiral :: Distance -> Int -> Distance -> Angle -> Command
spiral _ 0 _ _ = Sit
spiral side n step a
  | side < 0 = Sit
  | otherwise = Go side :#: Turn a :#: spiral (side + step) (n - 1) step a

-- 4. optimise
-- Remember that Go does not take negative arguments.

osplit :: Command -> [Command]
osplit Sit = [Sit]
osplit (Go a) = [Go a]
osplit (Turn a) = [Turn a]
osplit (x :#: y) = split x ++ split y

reconstruct :: [Command] -> Command
reconstruct [] = Sit
reconstruct xs
  | length xs == 1 = head xs
  | otherwise = head xs :#: reconstruct (drop 1 xs)

-- (Go 10 :#: Sit :#: Go 20 :#: Turn 35 :#: Go 0 :#: Turn 15 :#: Turn (-50))
optimise :: Command -> Command
optimise c
  | single_pass (osplit c) == c = c
  | otherwise = optimise (single_pass (osplit c))
  where
    single_pass l = reconstruct [pair_process (l !! n) (l !! (n + 1)) | n <- [0, 2 .. (length l - 2)]]

    pair_process (Turn x) (Turn y) = Turn (x + y)
    pair_process (Go x) (Go y) = Go (x + y)
    pair_process (Go 0) a = a
    pair_process (Turn 0) a = a
    pair_process a (Go 0) = a
    pair_process a (Turn 0) = a
    pair_process Sit Sit = Sit
    pair_process Sit a = a
    pair_process a Sit = a
    pair_process x y = x :#: y

-- ** Optional Material

-- L-Systems

-- 5. arrowhead
arrowhead :: Int -> Command
arrowhead = undefined

-- 6. snowflake
snowflake :: Int -> Command
snowflake = undefined

-- 7. hilbert
hilbert :: Int -> Command
hilbert = undefined

-- ** Challenge

-- Bonus L-Systems

peanoGosper :: Int -> Command
peanoGosper = undefined

cross :: Int -> Command
cross = undefined

branch :: Int -> Command
branch = undefined

thirtytwo :: Int -> Command
thirtytwo = undefined
