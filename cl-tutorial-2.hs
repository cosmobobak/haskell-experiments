--Exercise 1

--Each of the objects is an odd one out as they all have a unique property.

--Exercise 2

data Thing = A | B | C | D | E deriving (Eq, Show)

things :: [Thing]
things = [A, B, C, D, E]

-- These four properties characterise the objects:
data Colour = Orange | Blue deriving (Eq, Show)

data Shape = Square | Disc deriving (Eq, Show)

data Stroke = Thick | Thin deriving (Eq, Show)

data Size = Small | Big deriving (Eq, Show)

type Predicate = Thing -> Bool

isOrange, isBlue, isSquare, isDisc, isThickStroke, isThinStroke, isSmall, isBig :: Predicate
isOrange A = True
isOrange B = True
isOrange C = True
isOrange E = True
isOrange D = False
isBlue x = not (isOrange x)
isSquare A = True
isSquare B = True
isSquare D = True
isSquare E = True
isSquare C = False
isDisc x = not (isSquare x)
isThickStroke A = True
isThickStroke C = True
isThickStroke D = True
isThickStroke E = True
isThickStroke B = False
isThinStroke x = not (isThickStroke x)
isSmall A = True
isSmall B = True
isSmall C = True
isSmall D = True
isSmall E = False
isBig x = not (isSmall x)

--Exercise 3

--Changes made

--Exercise 4

thingsOtherThan :: Thing -> [Thing]
thingsOtherThan x = [n | n <- things, x /= n]

properties :: [Predicate]
properties = [isOrange, isBlue, isSquare, isDisc, isThickStroke, isThinStroke, isSmall, isBig]

propertiesOf :: Thing -> [Predicate]
propertiesOf x = [p | p <- properties, p x]

isPropertyOfAnotherThing :: Predicate -> Thing -> Bool
isPropertyOfAnotherThing p x = not (null [n | n <- thingsOtherThan x, p n])

propertiesOnlyOf :: Thing -> [Predicate]
propertiesOnlyOf o = [p | p <- properties, not (isPropertyOfAnotherThing p o)]

rank :: Thing -> Int
rank o = length [p | p <- properties, not (isPropertyOfAnotherThing p o)]

--the rank function is 1 for B through E, but 0 for A

--Exercise 5

f1 :: Bool
f1 = and [isThinStroke x | x <- things, isBlue x && isSquare x]

f2 :: Bool
f2 = or [not (isSquare x) | x <- things, isOrange x]

f3 :: Bool
f3 = and [isOrange x || isThickStroke x | x <- things, isBig x && isSquare x]

f4 :: Bool
f4 = or [not (isBig x) | x <- things, isOrange x && isDisc x]

--more complex statements

f5 :: Bool
f5 = not (or [isBlue x | x <- things, isSquare x])

f6 :: Bool
f6 = and [not (isBlue x) | x <- things, isSquare x]
