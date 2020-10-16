module SoundSyllogisms where

-- [ A Haskell encoding of syllogisms ] 

-- | Syllogisms are classified into four figures.
data Figure = F1 | F2 | F3 | F4 deriving (Eq, Show)
figures = [F1, F2, F3, F4]

-- | The premises and the conclusion of a syllogisms are of any of the following four types.
data Proposition = A | E | I | O deriving (Eq, Show)
propositions = [A, E, I, O]

type Syllogism = (Figure, Proposition, Proposition, Proposition)
syllogisms = [ (f, u, v, z) |
               f <- figures,
               u <- propositions,
               v <- propositions,
               z <- propositions ]

-- [ Support for Venn diagrams ]

-- | Each region of a Venn diagram is either empty (X), inhabited (V), or 'unknown' (N).
data Mark = X | V | N deriving (Eq, Show)

-- [ Operations with marks – needed for combining and relating Venn diagrams ] 

join :: Mark -> Mark -> Mark
V `join` _ = V
_ `join` V = V
X `join` o = o
o `join` X = o
N `join` N = N

meet :: Mark -> Mark -> Mark
X `meet` _ = X
_ `meet` X = X
V `meet` V = N
V `meet` N = N
N `meet` V = N
N `meet` N = N

             
minus :: Mark -> Mark -> Mark
X `minus` _ = X
o `minus` X = o
V `minus` V = N
V `minus` N = N
N `minus` V = N
N `minus` N = N

choose :: Mark -> Mark -> Mark
X `choose` _ = X
V `choose` _ = V
N `choose` X = X
N `choose` V = V
N `choose` N = N
               
implies :: Mark -> Mark -> Bool
X `implies` o = X == o
V `implies` o = V == o
N `implies` _ = True
          
-- | Venn diagrams with two circle have three regions.
type Venn2 = (Mark, Mark, Mark)

-- | Venn diagrams with two circle have seven regions.                
type Venn3 = (Mark, Mark, Mark, Mark, Mark, Mark, Mark)

-- | The following function corresponds to the combination of the two premisses of a syllogism.
combine :: Venn2 -> Venn2 -> Venn3
combine (minor_s, minor_sm, minor_m) (major_m, major_mp, major_p) = (m, s, p, sm, mp, sp, smp)
    where
      m = minor_m `meet` major_m
      s = minor_s `minus` major_p
      p = major_p `minus` minor_s
      sm = (major_m `minus` minor_m) `choose` (minor_sm `minus` major_mp)
      mp = (minor_m `minus` major_m) `choose` (major_mp `minus` minor_sm)
      sp = minor_s `meet` major_p
      smp = (minor_sm `minus` major_m) `choose` (major_mp `minus` minor_m)

-- | The function 'project' compiles the strongest conclusion of a syllogism.
project :: Venn3 -> Venn2
project (m, s, p, sm, mp, sp, smp) = (conc_s, conc_sp, conc_p)
    where
      conc_s = s `join` sm
      conc_sp = sp `join` smp
      conc_p = p `join` mp
          
-- | 'subdiagram' checks whether a given conclusion of a syllogism is weaker than another.
subdiagram :: Venn2 -> Venn2 -> Bool
subdiagram (s1, sp1, p1) (s2, sp2, p2) = and [s1 `implies` s2, sp1 `implies` sp2, p1 `implies` p2]
                                         
-- [ Encoding of syllogisms as Venn diagrams ]

-- | In the following 5 functions, m stands for 'middle', s for 'subject', and p for 'predicate'.
m_p :: Proposition -> Venn2
m_p A = (X, N, N)
m_p E = (N, X, N)
m_p I = (N, V, N)
m_p O = (V, N, N)

p_m :: Proposition -> Venn2
p_m A = (N, N, X)
p_m E = (N, X, N)
p_m I = (N, V, N)
p_m O = (N, N, V)

s_m :: Proposition -> Venn2
s_m A = (X, N, N)
s_m E = (N, X, N)
s_m I = (N, V, N)
s_m O = (V, N, N)

m_s :: Proposition -> Venn2
m_s A = (N, N, X)
m_s E = (N, X, N)
m_s I = (N, V, N)
m_s O = (N, N, V)

s_p :: Proposition -> Venn2
s_p A = (X, N, N)
s_p E = (N, X, N)
s_p I = (N, V, N)
s_p O = (V, N, N)

-- | Each syllogism is encoded as a triple of Venn diagrams with two circles.
-- The first two correspond to the premisses of the syllogisms, while the third one corresponds to the conclusion.
toVenn :: Syllogism -> (Venn2, Venn2, Venn2)
toVenn (F1, u, v, z) = (m_p u, s_m v, s_p z)
toVenn (F2, u, v, z) = (p_m u, s_m v, s_p z)
toVenn (F3, u, v, z) = (m_p u, m_s v, s_p z)
toVenn (F4, u, v, z) = (p_m u, m_s v, s_p z)

-- | Now we can check whether a given syllogism is correct.
correct :: Syllogism -> Bool
correct s = conclusion `subdiagram` (project (minor `combine`  major))
    where (major, minor, conclusion) = toVenn s

correctSyllogisms = filter correct syllogisms

expectedCorrect = [ (F1, A, A, A), (F1, A, I, I), (F1, E, A, E), (F1, E, I, O)
                  , (F2, A, E, E), (F2, A, O, O), (F2, E, A, E), (F2, E, I, O)
                  , (F3, A, I, I), (F3, E, I, O), (F3, I, A, I), (F3, O, A, O)
                  , (F4, A, E, E), (F4, E, I, O), (F4, I, A, I) ]

-- Finally, to convince ourselves that the above syllogisms are indeed all the correct ones, it suffices to check that the lists expectedCorrect and correctSyllogisms have the same elements.
