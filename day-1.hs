-- Day 1
import Control.Monad.State
import Data.Set (Set, empty, insert, member)

loadInput = readFile "inputs/day-1a.txt"

fixPositives num@(n:ns) = if n == '+' then ns else num

parse :: String -> [Integer]
parse = map (read . fixPositives) . lines

type Freq = Integer

frequency :: [Integer] -> Freq
frequency = sum

firstDupeFreq :: [Integer] -> Freq
firstDupeFreq ns = evalState (loop ns) (empty, 0)

type Freqs = Set Freq
type FreqsState = State (Freqs, Freq)

updateFreqState :: Freq -> (Freqs, Freq) -> (Freqs, Freq)
updateFreqState freq (freqs, _) = (insert freq freqs, freq)

step :: Integer -> FreqsState (Maybe Freq)
step n = do
    (freqs, lastFreq) <- get
    let nextFreq = lastFreq + n
    if nextFreq `member` freqs then
        return $ Just nextFreq
    else do
        modify $ updateFreqState nextFreq
        return Nothing

loop :: [Integer] -> FreqsState Freq
loop [] = error "No results"
loop (n:ns) = do
    res <- step n
    case res of
        Just freq -> return freq
        Nothing -> loop ns

main :: IO ()
main = do
    input <- parse <$> loadInput
    print $ frequency input
    print $ firstDupeFreq (cycle input)
