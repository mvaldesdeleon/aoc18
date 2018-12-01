-- Day 1

loadInput = readFile "inputs/day-1a.txt"

fixPositives num@(n:ns) = if n == '+' then ns else num

parse :: String -> [Integer]
parse = map (read . fixPositives) . lines

frequency :: [Integer] -> Integer
frequency = sum

firstDupeFreq :: [Integer] -> Integer
firstDupeFreq = undefined

main :: IO ()
main = do
    input <- parse <$> loadInput
    print $ frequency input
    print $ firstDupeFreq input
