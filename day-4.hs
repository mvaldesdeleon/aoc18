{-# LANGUAGE RecordWildCards #-}

import           Text.Parsec

loadInput :: IO String
loadInput = readFile "inputs/day-4.txt"

data Event
    = ShiftStart Integer
    | FallAsleep
    | WakeUp
    deriving (Show)

data Timestamp = Timestamp
    { tsYear   :: Integer
    , tsMonth  :: Integer
    , tsDay    :: Integer
    , tsHour   :: Integer
    , tsMinute :: Integer
    } deriving (Show)

data Entry = Entry
    { entryTimestamp :: Timestamp
    , entryEvent     :: Event
    } deriving (Show)

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

idTimesMinutes :: [Entry] -> Integer
idTimesMinutes = undefined

main :: IO ()
main = do
    input <- parseInput <$> loadInput
    print $ idTimesMinutes input
