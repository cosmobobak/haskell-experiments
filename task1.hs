
data Flip x = F Float
            | U Float
    deriving (Show, Eq)

(###) :: Flip x -> Flip x -> Flip x
U a ### F b = U (a / b)
F a ### U b = U (b / a)
U a ### U b = U (a * b)
F a ### F b = F (a * b)