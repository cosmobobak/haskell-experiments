envs ["s","t"]
      =  [("s",b):e | b <- [False, True], e <- envs ["t"]]
      =  [("s",False):e | e <- envs ["t"]] ++ [("s",True) | e <- envs ["t"]]
      =  [("s",False):e | e <- [("t",b):e | b <- [False, True], e <- envs []]] ++
         [("s",True ):e | e <- [("t",b):e | b <- [False, True], e <- envs []]]
      =  [("s",False):e | e <- ([("t",False):e | e <- envs []] ++ [("t",True):e | e <- envs []])] ++
         [("s",True ):e | e <- ([("t",False):e | e <- envs []] ++ [("t",True):e | e <- envs []])]
      =  [("s",False):e | e <- ([("t",False):e | e <- [[]]] ++ [("t",True):e | e <- [[]]])] ++
         [("s",True ):e | e <- ([("t",False):e | e <- [[]]] ++ [("t",True):e | e <- [[]]])]
      =  [("s",False):e | e <- ([("t",False):[]] ++ [("t",True):[]])] ++
         [("s",True ):e | e <- ([("t",False):[]] ++ [("t",True):[]])]
      =  [("s",False):e | e <- [("t",False):[], ("t",True):[]]] ++
         [("s",True ):e | e <- [("t",False):[], ("t",True):[]]]
      =  [("s",False):("t",False):[]] ++ [("s",False):("t",True):[]] ++
         [("s",True ):("t",False):[]] ++ [("s",True ):("t",True):[]]
      =  [ [("s",False),("t",False)], [("s",False),("t",True)], 
           [("s",True ),("t",False)], [("s",True ),("t",True)] ]