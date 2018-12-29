{-# LANGUAGE TemplateHaskell #-}

module Day25
    ( day25
    ) where

import           Control.Lens
import           Data.List       ((\\))
import qualified Data.Map.Strict as M
import           Data.Maybe      (fromMaybe)
import qualified Data.Set        as S
import           Paths_aoc18     (getDataFileName)
import           Text.Parsec     (Parsec, char, digit, many1, newline, option,
                                  parse, sepBy1)

data Point = Point
    { _x :: Integer
    , _y :: Integer
    , _z :: Integer
    , _t :: Integer
    } deriving (Show, Eq, Ord)

makeLenses ''Point

distance :: Point -> Point -> Integer
distance pa pb =
    abs (pb ^. x - pa ^. x) + abs (pb ^. y - pa ^. y) + abs (pb ^. z - pa ^. z) +
    abs (pb ^. t - pa ^. t)

parsePoint :: Parsec String () Point
parsePoint =
    Point <$> number <* char ',' <*> number <* char ',' <*> number <* char ',' <*>
    number
  where
    number = read <$> ((:) <$> option ' ' (char '-') <*> many1 digit)

parseInput :: String -> [Point]
parseInput input =
    case parse (parsePoint `sepBy1` newline) "" input of
        Left e   -> error $ show e
        Right ps -> ps

loadInput :: IO String
loadInput = getDataFileName "inputs/day-25.txt" >>= readFile

type Graph = M.Map Point [Point]

constellations :: [Point] -> [[Point]]
constellations ps = buildConstellations [] (S.fromList ps)
  where
    graph = foldl addPoint M.empty ps
    addPoint gr p = foldl (addNeighbor p) gr ps
    addNeighbor p gr np =
        if distance p np <= 3
            then M.insertWith (++) p [np] gr
            else gr
    buildConstellations :: [[Point]] -> S.Set Point -> [[Point]]
    buildConstellations cs s =
        if S.null s
            then cs
            else let c = nextConstellation s
                  in buildConstellations
                         (c : cs)
                         (S.difference s (S.fromList c))
    nextConstellation :: S.Set Point -> [Point]
    nextConstellation s =
        let p = S.findMin s
         in bfs [] [p]
    bfs :: [Point] -> [Point] -> [Point]
    bfs rs st =
        if null st
            then rs
            else let (s:ss) = st
                  in bfs (s : rs)
                         (ss ++ (fromMaybe [] (s `M.lookup` graph) \\ rs))

day25 :: IO ()
day25 = do
    input <- parseInput <$> loadInput
    print $ length . constellations $ input
