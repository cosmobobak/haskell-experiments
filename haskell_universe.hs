data Thing = R | S | T | U | V | W | X | Y | Z 

things = [R, S, T, U, V, W, X, Y, Z]

data Colour = Red | Blue | Amber 
data Shape = Disc | Triangle
data Size = Big | Small

type Predicate = Thing -> Bool
isRed, isBlue, isAmber, isDisc, isTriangle, isSmall, isBig :: Predicate

isDisc R = True
isDisc U = True
isDisc Y = True
isDisc Z = True
isDisc _ = False 

isTriangle x = not (isDisc x)

isRed U = True
isRed V = True
isRed _ = False

isBlue T = True
isBlue X = True
isBlue Z = True
isBlue _ = False

isAmber x = not (isRed x || isBlue x)

isSmall R = True
isSmall S = True
isSmall V = True
isSmall X = True
isSmall _ = False

isBig = not . isSmall

-- |This allows printing of the objects R through Z
instance Show Thing where
show R = "R"
show S = "S"
show T = "T"
show U = "U"
show V = "V"
show W = "W"
show X = "X"
show Y = "Y"
show Z = "Z"
