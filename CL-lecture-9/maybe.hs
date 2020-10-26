power :: Maybe Int -> Int -> Int
power Nothing n   =  2 ^ n
power (Just m) n  =  m ^ n

divide :: Int -> Int -> Maybe Int
divide n 0  =  Nothing
divide n m  =  Just (n `div` m)

-- this is ill-typed
-- wrong :: Int -> Int -> Int
-- wrong n m  =  divide n m + 3

-- this is how to write it
right :: Int -> Int -> Int
right n m  =  case divide n m of
                Nothing -> 3
                Just r -> r + 3
