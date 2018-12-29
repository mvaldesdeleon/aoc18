{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Day18
    ( day18
    ) where

import           Control.Concurrent (threadDelay)
import           Control.Lens
import           Data.List          (genericLength)
import qualified Data.Map.Strict    as M
import           Data.Maybe         (mapMaybe)
import           Paths_aoc18        (getDataFileName)

data Acre
    = OpenGround
    | Trees
    | LumberYard
    deriving (Show, Eq, Ord)

charToAcre :: Char -> Acre
charToAcre =
    \case
        '.' -> OpenGround
        '|' -> Trees
        '#' -> LumberYard

acreToChar :: Acre -> Char
acreToChar =
    \case
        OpenGround -> '.'
        Trees -> '|'
        LumberYard -> '#'

data Size = Size
    { _width  :: Integer
    , _height :: Integer
    } deriving (Show, Eq, Ord)

data Position = Position
    { _x :: Integer
    , _y :: Integer
    } deriving (Show, Eq, Ord)

makeLenses ''Size

makeLenses ''Position

type Acres = M.Map Position Acre

data Area = Area
    { _acres :: Acres
    , _size  :: Size
    } deriving (Eq, Ord)

instance Show Area where
    show Area {..} = unlines . map row $ [0 .. height - 1]
      where
        Size width height = _size
        row y = [char $ Position x y | x <- [0 .. width - 1]]
        char p = acreToChar $ _acres M.! p

makeLenses ''Area

loadInput :: IO String
loadInput = getDataFileName "inputs/day-18.txt" >>= readFile

mapSnd :: (a -> b) -> (c, a) -> (c, b)
mapSnd f (c, a) = (c, f a)

fI :: (Integral a, Num b) => a -> b
fI = fromIntegral

parseArea :: Size -> String -> Area
parseArea size input = Area acres size
  where
    acres = M.fromList . map (mapSnd charToAcre) . addPosition size $ input

addPosition :: Size -> String -> [(Position, Char)]
addPosition (Size width height) =
    zip [Position x y | y <- [0 .. height - 1], x <- [0 .. width - 1]]

parseInput :: String -> Area
parseInput input = parseArea size clean
  where
    size = Size width height
    width = genericLength . (!! 0) . lines $ input
    height = genericLength . lines $ input
    clean = filter (/= '\n') input

adjacentAcres :: Acres -> Position -> [Acre]
adjacentAcres as = mapMaybe (`M.lookup` as) . adjacentPositions

adjacentPositions :: Position -> [Position]
adjacentPositions Position {..} =
    [ Position (_x - 1) (_y - 1)
    , Position _x (_y - 1)
    , Position (_x + 1) (_y - 1)
    , Position (_x - 1) _y
    , Position (_x + 1) _y
    , Position (_x - 1) (_y + 1)
    , Position _x (_y + 1)
    , Position (_x + 1) (_y + 1)
    ]

count :: [Acre] -> Acre -> Integer
count as a = genericLength . filter (== a) $ as

oneMinute :: Area -> Area
oneMinute = over acres (\as -> M.mapWithKey (nextAcreAt as) as)
  where
    nextAcreAt :: Acres -> Position -> Acre -> Acre
    nextAcreAt as p =
        \case
            OpenGround ->
                if countAdjacent Trees >= 3
                    then Trees
                    else OpenGround
            Trees ->
                if countAdjacent LumberYard >= 3
                    then LumberYard
                    else Trees
            LumberYard ->
                if countAdjacent LumberYard >= 1 && countAdjacent Trees >= 1
                    then LumberYard
                    else OpenGround
      where
        countAdjacent = count $ adjacentAcres as p

resourceValue :: Area -> Integer
resourceValue =
    views acres (((*) <$> (`count` Trees) <*> (`count` LumberYard)) . M.elems)

findCycle :: Ord a => [a] -> (Integer, Integer)
findCycle vs = go vs 0 M.empty
  where
    go :: Ord a => [a] -> Integer -> M.Map a Integer -> (Integer, Integer)
    go (v:vs) i vals =
        case v `M.lookup` vals of
            Just iv -> (iv, i - iv)
            Nothing -> go vs (i + 1) (M.insert v i vals)

animate :: [Area] -> IO ()
animate =
    mapM_
        (\a -> do
             print a
             threadDelay 100000)

day18 :: IO ()
day18 = do
    input <- parseInput <$> loadInput
    let areas = iterate oneMinute input
    -- animate areas
    print $ resourceValue (areas !! 10)
    let values = map resourceValue areas
    let (mu, lambda) = findCycle areas
    let target = 1000000000
    print $ values !! (((target - fI mu) `mod` fI lambda) + fI mu)
