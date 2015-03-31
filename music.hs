data Note = C | C' | D | D' | E | F | F' | G | G'| A | A' | B
            deriving (Show, Enum, Read, Bounded, Ord, Eq)
data Step = Base | Half | Whole deriving (Show, Enum, Read, Bounded, Ord, Eq)

data Interval = P1 | Mi2 | M2 | Mi3 | M3 | P4 | P5 | Mi6 | M6 | Mi7 | M7 | P8
            deriving (Show, Enum, Read, Bounded, Ord, Eq)

type Scale = [Note]

intervalBetween :: Note -> Note -> Interval
intervalBetween n m = toBoundEnum (fromEnum m - fromEnum n)

bound :: Int -> Int
bound n = n `mod` 12

toBoundEnum :: (Enum a) => Int -> a
toBoundEnum = toEnum.bound

adjustNoteByStep :: Note -> Step -> Note
adjustNoteByStep n s = toBoundEnum $ fromEnum n + fromEnum s

createScale :: [Step] -> Note -> Scale
createScale [] n = [n]
createScale (step:steps) n = [n] ++ createScale steps (adjustNoteByStep n step)

majorScale = createScale [Whole, Whole, Half, Whole, Whole, Whole]
minorScale = createScale [Whole, Half, Whole, Whole, Half, Whole]


