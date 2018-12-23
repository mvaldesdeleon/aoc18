{-# LANGUAGE TemplateHaskell #-}

import           Control.Lens
import           Data.Foldable  (length)
import           Data.Function  (on)
import qualified Data.List.Safe as L
import           Text.Parsec    (Parsec, between, char, digit, many1, newline,
                                 option, parse, sepBy, string)

loadInput :: IO String
loadInput = readFile "inputs/day-23.txt"

data Position = Position
    { _x :: Integer
    , _y :: Integer
    , _z :: Integer
    } deriving (Show, Eq)

makeLenses ''Position

distance :: Position -> Position -> Integer
distance pa pb =
    abs (pb ^. x - pa ^. x) + abs (pb ^. y - pa ^. y) + abs (pb ^. z - pa ^. z)

data NanoBot = NanoBot
    { _pos :: Position
    , _r   :: Integer
    } deriving (Show, Eq)

makeLenses ''NanoBot

strongest :: [NanoBot] -> Maybe NanoBot
strongest = L.maximumBy (compare `on` view r)

inRangeOf :: NanoBot -> Position -> Bool
inRangeOf n p = distance (n ^. pos) p <= n ^. r

parseNumber :: Parsec String () Integer
parseNumber = read <$> signedNumber
  where
    signedNumber = (:) <$> option ' ' (char '-') <*> many1 digit

parsePosition :: Parsec String () Position
parsePosition =
    Position <$> parseNumber <*> (char ',' *> parseNumber) <*>
    (char ',' *> parseNumber)

parseNanoBot :: Parsec String () NanoBot
parseNanoBot =
    NanoBot <$> between (string "pos=<") (string ">") parsePosition <*>
    (string ", r=" *> parseNumber)

parseInput :: String -> [NanoBot]
parseInput input =
    case parse (parseNanoBot `sepBy` newline) "" input of
        Left e   -> error $ show e
        Right ns -> ns

main :: IO ()
main = do
    input <- parseInput <$> loadInput
    let Just st = strongest input
    print $ length . filter (inRangeOf st . view pos) $ input
