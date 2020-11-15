f :: Int -> Int
f n = sum [i ^ 3 | i <- [1 .. n], even i]

g :: Int -> Int
g n = h 0 0
  where
    h :: Int -> Int -> Int
    h i x
      | i > n = x
      | even i = h (i + 1) (x + i ^ 3)
      | otherwise = h (i + 1) x

k :: Int -> Int
k n = (foldl (+) 0 . map (^ 3) . filter even) [1 .. n]