import Test.QuickCheck
import Control.Monad
import Data.List

type Name = String

-- using Prp rather than Prop to avoid conflict with Prop in QuickCheck
data Prp = Var Name
          | F
          | T
          | Not Prp
          | Prp :|: Prp
          | Prp :&: Prp
          deriving (Eq, Ord, Show)

type Names = [Name]
type Env = [(Name,Bool)]

showPrp :: Prp -> String
showPrp (Var x)   =  x
showPrp (F)       =  "F"
showPrp (T)       =  "T"
showPrp (Not p)   =  par ("~" ++ showPrp p)
showPrp (p :|: q) =  par (showPrp p ++ "|" ++ showPrp q)
showPrp (p :&: q) =  par (showPrp p ++ "&" ++ showPrp q)

par :: String -> String
par s  =  "(" ++ s ++ ")"

names :: Prp -> Names
names (Var x)    =  [x]
names (F)        =  []
names (T)        =  []
names (Not p)    =  names p
names (p :|: q)  =  nub (names p ++ names q)
names (p :&: q)  =  nub (names p ++ names q)

eval :: Env -> Prp -> Bool
eval e (Var x)        =  lookUp e x
eval e (F)            =  False
eval e (T)            =  True
eval e (Not p)        =  not (eval e p)
eval e (p :|: q)      =  eval e p || eval e q
eval e (p :&: q)      =  eval e p && eval e q

lookUp :: Eq a => [(a,b)] -> a -> b
lookUp xys x  =  the [ y | (x',y) <- xys, x == x' ]
  where
  the [x]  =  x

p0 :: Prp
p0 =  (Var "a" :&: Var "b") :|:
      (Not (Var "a") :&: Not (Var "b"))

p1 :: Prp
p1 =  (Var "a" :&: Not (Var "a"))

env0 :: Env
env0 =  [("a",False), ("b",False)]

test_Prp :: Bool
test_Prp =
  showPrp p0  ==  "((a&b)|((~a)&(~b)))" &&
  showPrp p1  ==  "(a&(~a))" &&
  names p0  ==  ["a","b"]  &&
  eval env0 p0  ==  True  &&
  lookUp env0 "a"  ==  False

envs :: Names -> [Env]
envs []      =  [[]]
envs (x:xs)  =  [ (x,b):e | b <- bs, e <- envs xs ]
  where
  bs = [False,True]

test_envs :: Bool
test_envs =
      envs []  ==
        [[]]
  &&  envs ["b"]  ==
        [[("b",False)],
         [("b",True )]]
  &&  envs ["a","b"]  ==
        [[("a",False),("b",False)],
         [("a",False),("b",True )],
         [("a",True ),("b",False)],
         [("a",True ),("b",True )]] 

satisfiable :: Prp -> Bool
satisfiable p  =  or [ eval e p | e <- envs (names p) ]

test_satisfiable :: Bool
test_satisfiable  =  satisfiable p0 && not (satisfiable p1)

-- allow QuickCheck to generate arbitrary values of type Prp
instance Arbitrary Prp where
  arbitrary = sized prp
    where
    prp 0  =
      oneof [return F,
             return T,
             liftM Var arbitrary]
    prp n | n > 0 =
      oneof [return F,
             return T,
             liftM Var arbitrary,
             liftM Not (prp (n-1)),
             liftM2 (:&:) (prp (n `div` 2)) (prp (n `div` 2)),
             liftM2 (:|:) (prp (n `div` 2)) (prp (n `div` 2))]
