import Data.List

-- | First we define expressions (with the predicates A, B, C needed
-- for checking the associativity of xor) and sequents.
data Exp
  = A
  | B
  | C
  | Xor Exp Exp
  deriving (Show, Eq)

data Sequent
  = [Exp] :|= [Exp]
  deriving (Show)

-- | The following three functions define encodings of the immediate
-- rule and of the left and right xor rules. Each function takes as
-- input a sequent, expressions (in the case of the xor rules), and
-- returns a list of sequents, as follows: the returned list contains
--
-- - the premises of the rule, if the rule can be applied, or
-- - the original input sequent, if the rule cannot be applied.
immediate :: Sequent -> [Sequent]
immediate (l :|= r) =
  if null (intersect l r)
    then [l :|= r]
    else []

xorL :: Exp -> Exp -> Sequent -> [Sequent]
xorL a b (l :|= r) =
  if (a `Xor` b) `elem` l
    then [(a : gamma) :|= (b : delta), (b : gamma) :|= (a : delta)]
    else [l :|= r]
  where
    gamma = delete (a `Xor` b) l
    delta = r

xorR :: Exp -> Exp -> Sequent -> [Sequent]
xorR a b (l :|= r) =
  if (a `Xor` b) `elem` r
    then [gamma :|= (a : b : delta), (a : b : gamma) :|= delta]
    else [l :|= r]
  where
    gamma = l
    delta = delete (a `Xor` b) r

-- | The function firstXor searches for the first occurrence of a
-- xor-expression in a list of expressions, and returns that
-- xor-expression. If no such expression exists in the list, it
-- returns Nothing.
firstXor :: [Exp] -> Maybe Exp
firstXor = foldl f Nothing
  where
    f Nothing (a `Xor` b) = Just (a `Xor` b)
    f Nothing _ = Nothing
    f (Just x) _ = Just x

-- | The function xorRed applies one reduction step according to the
-- xor rules to a sequent, producing a list of sequents. The returned
-- list contains:
--
-- - only the original sequent if no xor rule can be applied, or
-- - two sequents (matching the premises of a xor rule), otherwise.
xorRed :: Sequent -> [Sequent]
xorRed (l :|= r) =
  case firstXor l of
    Just (a `Xor` b) -> xorL a b (l :|= r)
    Nothing -> case firstXor r of
      Just (a `Xor` b) -> xorR a b (l :|= r)
      Nothing -> [l :|= r]

-- | red generalizes xorRed to lists of sequents and, in addition,
-- uses the immediate rule to discard trivial branches.
red :: [Sequent] -> [Sequent]
red = concat . map immediate . concat . map xorRed

-- | We can now define the sequents that we want to check:
xorAssocL = [[(A `Xor` B) `Xor` C] :|= [A `Xor` (B `Xor` C)]]

-- red $ red $ red $ red $ xorAssocL
-- produces []
xorAssocR = [[A `Xor` (B `Xor` C)] :|= [(A `Xor` B) `Xor` C]]

-- red $ red $ red $ red $ xorAssocR
-- produces []
