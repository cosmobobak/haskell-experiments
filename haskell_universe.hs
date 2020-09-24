data Thing = R | S | T | U | V | W | X | Y | Z 

things = [R, S, T, U, V, W, X, Y, Z]

data Colour = Red | Blue | Amber 
data Shape = Disc | Triangle
data Size = Big | Small

type Predicate = Thing -> Bool
isRed, isBlue, isAmber, isDisk, isTriangle, isSmall, isBig :: Predicate

isDisk R = True
isDisk U = True
isDisk Y = True
isDisk Z = True
isDisk _ = False 

isTriangle x = not (isDisk x)

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
