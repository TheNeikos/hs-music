data Note = C | C' | D | D' | E | F | F' | G | G'| A | A' | B
            deriving (Show, Enum, Read, Bounded, Ord, Eq)
data Step = Base | Half | Whole deriving (Show, Enum, Read, Bounded, Ord, Eq)

type Scale = [Note]

bound :: Int -> Int
bound n = n `mod` 12

toBoundEnum = toEnum.bound

adjustNoteByStep :: Note -> Step -> Note
adjustNoteByStep n s = toBoundEnum $ fromEnum n + fromEnum s

createScale :: [Step] -> Note -> Scale
createScale [] n = [n]
createScale (step:steps) n = [n] ++ createScale steps (adjustNoteByStep n step)

majorScale = createScale [Whole, Whole, Half, Whole, Whole, Whole]
minorScale = createScale [Whole, Half, Whole, Whole, Half, Whole]
