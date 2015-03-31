import Codec.Midi

data Note = C | C' | D | D' | E | F | F' | G | G'| A | A' | B
            deriving (Show, Enum, Read, Bounded, Ord, Eq)
data Step = Base | Half | Whole deriving (Show, Enum, Read, Bounded, Ord, Eq)

data Interval = P1 | Mi2 | M2 | Mi3 | M3 | P4 | P5 | Mi6 | M6 | Mi7 | M7 | P8
            deriving (Show, Enum, Read, Bounded, Ord, Eq)

data Beat = WholeNote | HalfNote | Quarter | Eighth | Sixteenth
            deriving (Show, Enum, Read, Bounded, Ord, Eq)

data FullNote = FullNote Note Octave
            deriving (Show, Read, Ord, Eq)

type Scale = [FullNote]
type Octave = Int
type Measure = [(Beat, FullNote)]
type Piece = [Measure]


intervalBetween :: Note -> Note -> Interval
intervalBetween n m = toBoundEnum (fromEnum m - fromEnum n)

bound :: Int -> Int
bound n = n `mod` 12

toBoundEnum :: (Enum a) => Int -> a
toBoundEnum = toEnum.bound

toFullNote :: Int -> FullNote
toFullNote n = FullNote (toBoundEnum n) (n `div` 12)

adjustNoteByStep :: FullNote -> Step -> FullNote
adjustNoteByStep n s = toFullNote $ fromFullNote n + fromEnum s

fromFullNote :: FullNote -> Int
fromFullNote (FullNote n m) = fromEnum n + m * 12

fromBeat :: Beat -> Int
fromBeat b = 24 `div` (2 ^ fromEnum b :: Int)

createScale :: [Step] -> FullNote -> Scale
createScale [] n = [n]
createScale (step:steps) n = [n] ++ createScale steps (adjustNoteByStep n step)

majorScale = createScale [Whole, Whole, Half, Whole, Whole, Whole, Half]
minorScale = createScale [Whole, Half, Whole, Whole, Half, Whole, Half]

measureToTrack :: Measure -> [(Ticks, Message)]
measureToTrack m = innerConvert m 0 where
    innerConvert []     t = [(t, TrackEnd)]
    innerConvert (x:xs) t = [(t, NoteOn 2 note 80)]
                              ++ [(beat, NoteOff 2 note 0)]
                              ++ innerConvert xs beat
        where
        beat = fromBeat (fst x)
        note = fromFullNote (snd x) + 60

pieceToMidi m = Midi {
            fileType = MultiTrack,
            tracks = map measureToTrack m,
            timeDiv = TicksPerBeat 24 }
