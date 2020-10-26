import Data.List

type Name = String

data  Univ  =  UBool Bool
	    |  UInt Int
            |  UList [Univ]
	    |  UFun (Univ -> Univ)

data  Hask  =  HTrue
	    |  HFalse
            |  HIf Hask Hask Hask
            |  HLit Int
            |  HEq Hask Hask
            |  HAdd Hask Hask
	    |  HVar Name
	    |  HLam Name Hask
	    |  HApp Hask Hask

type  HEnv  =  [(Name, Univ)]

showUniv :: Univ -> String
showUniv (UBool b)   =  show b
showUniv (UInt i)    =  show i
showUniv (UList us)  =
  "[" ++ concat (intersperse "," (map showUniv us)) ++ "]"

eqUniv :: Univ -> Univ -> Bool
eqUniv (UBool b) (UBool c)    =  b == c
eqUniv (UInt i) (UInt j)      =  i == j
eqUniv (UList us) (UList vs)  =  and [ eqUniv u v | (u,v) <- zip us vs ]
eqUniv u v                    =  False

hEval :: Hask -> HEnv -> Univ
hEval HTrue r         =  UBool True
hEval HFalse r        =  UBool False
hEval (HIf c d e) r   =
  hif (hEval c r) (hEval d r) (hEval e r)
  where  hif (UBool b) v w  =  if b then v else w
hEval (HLit i) r      =  UInt i
hEval (HEq d e) r     =  heq (hEval d r) (hEval e r)
  where  heq (UInt i) (UInt j) = UBool (i == j)
hEval (HAdd d e) r    =  hadd (hEval d r) (hEval e r)
  where  hadd (UInt i) (UInt j) = UInt (i + j)
hEval (HVar x) r      =  lookUp r x
hEval (HLam x e) r    =  UFun (\v -> hEval e ((x,v):r))
hEval (HApp d e) r    =  happ (hEval d r) (hEval e r)
  where  happ (UFun f) v  =  f v

lookUp :: Eq a => [(a,b)] -> a -> b
lookUp xys x  =  the [ y | (x',y) <- xys, x == x' ]
  where
  the [x]  =  x

h0 =
 (HApp
   (HApp
     (HLam "x" (HLam "y" (HAdd (HVar "x") (HVar "y"))))
     (HLit 3))
   (HLit 4))

test_h0 = eqUniv (hEval h0 []) (UInt 7)
