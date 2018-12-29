module Day1
    ( day1
    ) where

import           Control.Monad.State
import           Data.Set            (Set, empty, insert, member)
import           Paths_aoc18         (getDataFileName)

loadInput :: IO String
loadInput = getDataFileName "inputs/day-1.txt" >>= readFile

fixPositives :: String -> String
fixPositives num@(n:ns) =
    if n == '+'
        then ns
        else num

parseInput :: String -> [Integer]
parseInput = map (read . fixPositives) . lines

type Freq = Integer

frequency :: [Integer] -> Freq
frequency = sum

firstDupeFreq :: [Integer] -> Freq
firstDupeFreq ns = evalState (findDupe $ cycle ns) (empty, 0)

type Freqs = Set Freq

type FreqsState = State (Freqs, Freq)

advanceAndEval :: Integer -> FreqsState (Maybe Freq)
advanceAndEval n = do
    (freqs, lastFreq) <- get
    let nextFreq = lastFreq + n
    if nextFreq `member` freqs
        then do
            put (freqs, nextFreq)
            return $ Just nextFreq
        else do
            put (insert nextFreq freqs, nextFreq)
            return Nothing

findDupe :: [Integer] -> FreqsState Freq
findDupe [] = error "No results"
findDupe (n:ns) = do
    res <- advanceAndEval n
    case res of
        Just freq -> return freq
        Nothing   -> findDupe ns

day1 :: IO ()
day1 = do
    input <- parseInput <$> loadInput
    print $ frequency input
    print $ firstDupeFreq input
