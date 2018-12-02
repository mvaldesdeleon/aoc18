import Control.Monad.State
import Data.Set (Set, empty, insert, member)

loadInput = readFile "inputs/day-1.txt"

fixPositives num@(n:ns) = if n == '+' then ns else num

parse :: String -> [Integer]
parse = map (read . fixPositives) . lines

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
    if nextFreq `member` freqs then do
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
        Nothing -> findDupe ns

main :: IO ()
main = do
    input <- parse <$> loadInput
    print $ frequency input
    print $ firstDupeFreq input
