import Data.List

loadInput = readFile "inputs/day-2.txt"

type ID = String

parse :: String -> [ID]
parse = lines

checksumParams :: ID -> (Bool, Bool)
checksumParams id = (2 `elem` freqs, 3 `elem` freqs)
    where freqs = map length . group . sort $ id

addParams :: (Integer, Integer) -> (Bool, Bool) -> (Integer, Integer)
addParams (a, b) (x, y) = (if x then succ a else a, if y then succ b else b)

mult :: (Integer, Integer) -> Integer
mult = uncurry (*)

checksum :: [String] -> Integer
checksum = mult . foldl addParams (0, 0) . map checksumParams

main :: IO ()
main = do
    input <- parse <$> loadInput
    print $ checksum input
