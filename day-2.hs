import Data.List

loadInput = readFile "inputs/day-2.txt"

type ID = String

parse :: String -> [ID]
parse = lines

checksumParams :: ID -> (Bool, Bool)
checksumParams id = (2 `elem` freqs, 3 `elem` freqs)
    where freqs = map length . group . sort $ id

addParams :: (Integer, Integer) -> (Bool, Bool) -> (Integer, Integer)
addParams (n, m) (p, q) = (if p then succ n else n, if q then succ m else m)

mult :: (Integer, Integer) -> Integer
mult = uncurry (*)

checksum :: [String] -> Integer
checksum = mult . foldl addParams (0, 0) . map checksumParams

consecutiveDupe :: Ord a => [a] -> Maybe a
consecutiveDupe (a:b:cs) = if a == b then Just a else consecutiveDupe (b:cs)
consecutiveDupe _ = Nothing

dropNth :: Integer -> [a] -> [a]
dropNth _ [] = []
dropNth n (a:as)
    | n < 0 = (a:as)
    | n == 0 = as
    | otherwise = a : dropNth (n - 1) as

idSize :: [ID] -> Integer
idSize = toInteger . length . head

testPosition :: Integer -> [ID] -> Maybe ID
testPosition n = consecutiveDupe . sort . map (dropNth (n - 1))

findCommon :: [ID] -> ID
findCommon ids = findFrom max
    where max = idSize ids
          findFrom 0 = error "No results"
          findFrom n = case testPosition n ids of
            Just id -> id
            Nothing -> findFrom (n - 1)

main :: IO ()
main = do
    input <- parse <$> loadInput
    print $ checksum input
    print $ findCommon input
