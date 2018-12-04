{-# LANGUAGE RecordWildCards #-}

import           Data.Function
import           Data.List
import           Text.Parsec

loadInput :: IO String
loadInput = readFile "inputs/day-4.txt"

data Event
    = ShiftStart Integer
    | FallAsleep
    | WakeUp
    deriving (Show, Eq)

data Timestamp = Timestamp
    { tsYear   :: Integer
    , tsMonth  :: Integer
    , tsDay    :: Integer
    , tsHour   :: Integer
    , tsMinute :: Integer
    } deriving (Show, Eq, Ord)

data Entry = Entry
    { entryTimestamp :: Timestamp
    , entryEvent     :: Event
    } deriving (Show, Eq)

instance Ord Entry where
    a <= b = entryTimestamp a <= entryTimestamp b

-- Parsing stuff
integer :: Parsec String () Integer
integer = read <$> many digit

timestamp :: Parsec String () Timestamp
timestamp =
    Timestamp <$> (char '[' *> integer <* char '-') <*> (integer <* char '-') <*>
    (integer <* char ' ') <*>
    (integer <* char ':') <*>
    (integer <* char ']')

event :: Parsec String () Event
event = char ' ' *> (shiftStart <|> fallsAsleep <|> wakesUp)
  where
    shiftStart =
        ShiftStart <$> (string "Guard #" *> integer <* string " begins shift")
    fallsAsleep = FallAsleep <$ string "falls asleep"
    wakesUp = WakeUp <$ string "wakes up"

entry :: Parsec String () Entry
entry = Entry <$> timestamp <*> event <* optional (char '\n')

parseInput :: String -> [Entry]
parseInput input =
    case result of
        Left e        -> error $ show e
        Right entries -> entries
  where
    result = parse (many entry) "" input

-- Actual problem
data SleepInterval = SleepInterval
    { siGuardId  :: Integer
    , siFrom     :: Timestamp
    , siTo       :: Timestamp
    , siDuration :: Integer
    } deriving (Show)

sleepIntervals :: [Entry] -> [SleepInterval]
sleepIntervals = start . sort
  where
    start (Entry {..}:es) =
        case entryEvent of
            ShiftStart guardId -> intervals guardId es
            _                  -> error "Expected ShiftStart"
    start [] = []
    intervals guardId (f:t:es) =
        case (entryEvent f, entryEvent t) of
            (FallAsleep, WakeUp) ->
                let tsFrom = entryTimestamp f
                    tsTo = entryTimestamp t
                in SleepInterval
                       guardId
                       tsFrom
                       tsTo
                       (tsMinute tsTo - tsMinute tsFrom) :
                   intervals guardId es
            (ShiftStart guardId, _) -> intervals guardId (t : es)
            _ -> error "Expected FallAsleep:WakeUp"
    intervals _ [] = []
    intervals _ _ = error "Unbalanced entries"

data Guard = Guard
    { guardId             :: Integer
    , guardSleepIntervals :: [SleepInterval]
    , guardSleepDuration  :: Integer
    } deriving (Show)

guards :: [Entry] -> [Guard]
guards = map mkGuard . groups . sleepIntervals
  where
    groups = groupBy ((==) `on` siGuardId) . sortOn siGuardId
    mkGuard guardIntervals =
        Guard
            (siGuardId . head $ guardIntervals)
            guardIntervals
            (sum . map siDuration $ guardIntervals)

sleepyGuard = maximumBy (compare `on` guardSleepDuration) . guards

idTimesMinutes :: [Entry] -> Integer
idTimesMinutes entries = guardId guard * minute
  where
    guard = sleepyGuard entries
    minute = sleepyMinute . guardSleepIntervals $ guard

sleepyMinute :: [SleepInterval] -> Integer
sleepyMinute sleepIntervals = fst . maximumBy (compare `on` snd) $ scanList
  where
    toBorders SleepInterval {..} = [(tsMinute siFrom, 1), (tsMinute siTo, -1)]
    borders = sortBy (compare `on` fst) $ sleepIntervals >>= toBorders
    scanList = tail . scanl sum empty $ borders
    sum (_, n) (x, m) = (x, n + m)
    empty = (0, 0)

main :: IO ()
main = do
    input <- parseInput <$> loadInput
    print $ idTimesMinutes input
