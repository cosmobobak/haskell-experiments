data Exp  =  Lit Int
          |  Add Exp Exp
          |  Mul Exp Exp

evalExp :: Exp -> Int
evalExp (Lit n)    =  n
evalExp (Add e f)  =  evalExp e + evalExp f
evalExp (Mul e f)  =  evalExp e * evalExp f

showExp :: Exp -> String
showExp (Lit n)    =  show n
showExp (Add e f)  =  par (showExp e ++ "+" ++ showExp f)
showExp (Mul e f)  =  par (showExp e ++ "*" ++ showExp f)

par :: String -> String
par s  =  "(" ++ s ++ ")"

e0, e1 :: Exp
e0 = Add (Lit 2) (Mul (Lit 3) (Lit 3))
e1 = Mul (Add (Lit 2) (Lit 3)) (Lit 3)

test_Exp :: Bool
test_Exp =
      showExp e0 == "(2+(3*3))"
  &&  evalExp e0 == 11
  &&  showExp e1 == "((2+3)*3)"
  &&  evalExp e1 == 15

--evalExp ((Lit 3 `Mul` Lit 4) `Mul` Lit 5)
--exalExp Mul (Lit 3 `Mul` Lit 4) (Lit 5)
--(evalExp (Lit 3 `Mul` Lit 4)) * (evalExp (Lit 5))
--(evalExp (Lit 3) * evalExp (Lit 4))) * (5)
--(3 * 4) * (5)
--(12) * (5)
--60

--showExp ((Lit 3 `Mul` Lit 4) `Mul` Lit 5)
--par (showExp (Lit 3 `Mul` Lit 4) ++ "*" ++ showExp Lit 5)
--par (par (showExp Lit 3 ++ "*" ++ showExp Lit 4) ++ "*" ++ "5")
--par (par ("3" ++ "*" ++ "4") ++ "*" ++ "5")
--par (par ("3*4") ++ "*" ++ "5")
--par ("(3*4)" ++ "*" ++ "5")
--par ("(3*4)*5")
--"((3*4)*5)"