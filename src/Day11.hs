module Day11
    ( day11
    ) where

import           Data.Char       (digitToInt)
import           Data.Function
import           Data.List
import qualified Data.List.Safe  as L
import qualified Data.Map.Strict as M
import           Data.Maybe      (fromMaybe)
import           Paths_aoc18     (getDataFileName)

loadInput :: IO String
loadInput = getDataFileName "inputs/day-11.txt" >>= readFile

parseInput :: String -> Integer
parseInput = read

powerLevel :: Integer -> (Integer, Integer) -> Integer
powerLevel gridSerial (x, y) =
    subtract 5 . hundreds $ (rackId * y + gridSerial) * rackId
  where
    rackId = x + 10

hundreds :: Integer -> Integer
hundreds = fromIntegral . digitToInt . fromMaybe '0' . (L.!! 2) . reverse . show

totalPower3 :: Integer -> (Integer, Integer) -> Integer
totalPower3 gridSerial (left, top) =
    sum . map (powerLevel gridSerial) $ positions
  where
    positions = [(x, y) | x <- [left .. left + 2], y <- [top .. top + 2]]

maxTotalPower3 :: Integer -> (Integer, Integer)
maxTotalPower3 gridSerial = snd . maximumBy (compare `on` fst) $ options
  where
    options =
        [ (totalPower3 gridSerial (x, y), (x, y))
        | x <- [1 .. 300]
        , y <- [1 .. 300]
        ]

maxTotalPower :: Integer -> (Integer, Integer, Integer)
maxTotalPower gridSerial = snd . maximumBy (compare `on` fst) $ options
  where
    options =
        [ totalPower z (x, y)
        | z <- [1 .. 300]
        , x <- [1 .. 301 - z]
        , y <- [1 .. 301 - z]
        ]
    get = M.findWithDefault 0
    sumGrid =
        foldl buildSums M.empty [(x, y) | y <- [1 .. 300], x <- [1 .. 300]]
    buildSums m (x, y) =
        M.insert
            (x, y)
            (powerLevel gridSerial (x, y) + get (x - 1, y) m + get (x, y - 1) m -
             get (x - 1, y - 1) m)
            m
    totalPower z (x, y) =
        ( get (x + z - 1, y + z - 1) sumGrid - get (x - 1, y + z - 1) sumGrid -
          get (x + z - 1, y - 1) sumGrid +
          get (x - 1, y - 1) sumGrid
        , (fromIntegral x, fromIntegral y, fromIntegral z))

day11 :: IO ()
day11 = do
    input <- parseInput <$> loadInput
    print $ maxTotalPower3 input
    print $ maxTotalPower input
