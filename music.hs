
data Pitch = Pitch Float deriving (Show, Read, Ord, Eq)
data Note = C | C' | D | D' | E | F | F' | G | G'| A | A' | B
            deriving (Show, Enum, Read, Bounded, Ord, Eq)
data Step = Base | Half | Whole deriving (Show, Enum, Read, Bounded, Ord, Eq)

type Scale = [Note]


nextNote :: Note -> Note
nextNote B = C
nextNote n = succ n

bound :: Int -> Int
bound n = n `mod` 12

toBoundEnum = toEnum.bound

adjustNote :: Note -> Step -> Note
adjustNote n s = toBoundEnum $ fromEnum n + fromEnum s

createScale :: [Step] -> Note -> Scale
createScale [] n = [n]
createScale (x:xs) n = [n] ++ createScale xs (adjustNote n x)

majorScale = createScale [Whole, Whole, Half, Whole, Whole, Whole]
minorScale = createScale [Whole, Half, Whole, Whole, Half, Whole]
