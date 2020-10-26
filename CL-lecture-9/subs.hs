subs :: [a] -> [[a]]
subs []      =  [[]]
subs (x:xs)  =  subs xs ++ [ x:ys | ys <- subs xs ]

test_subs :: Bool
test_subs =
      subs []  ==
        [[] :: [String]]
  &&  subs ["b"]  ==
        [[], ["b"]]
  &&  subs ["a","b"] ==
        [[], ["b"], ["a"], ["a","b"]]
