import           Data.Char
import           Data.List.Zipper

loadInput :: IO String
loadInput = readFile "inputs/day-5.txt"

parseInput :: String -> String
parseInput = filter isAlpha

units :: [Char] -> Integer
units = fromIntegral . length . toList . reduceRight . fromList

reduceRight :: Zipper Char -> Zipper Char
reduceRight z =
    case (mv, mn) of
        (Just v, Just n) ->
            if match v n
                then reduceLeft $ delete . delete $ z
                else reduceLeft z
        _ -> z
  where
    nz = right z
    mv = safeCursor z
    mn = safeCursor nz

reduceLeft :: Zipper Char -> Zipper Char
reduceLeft z =
    if beginp z
        then reduceRight $ right z
        else case (mv, mp) of
                 (Just v, Just p) ->
                     if match v p
                         then reduceLeft $ delete . pop $ z
                         else reduceRight $ right z
                 _ -> reduceRight $ right z
  where
    pz = left z
    mv = safeCursor z
    mp = safeCursor pz

match :: Char -> Char -> Bool
match a b = a /= b && toLower a == toLower b

leastUnits :: [Char] -> Integer
leastUnits input = minimum [units $ removeUnit u input | u <- ['a' .. 'z']]

removeUnit :: Char -> [Char] -> [Char]
removeUnit u = filter ((/=) (toLower u)) . filter ((/=) (toUpper u))

main :: IO ()
main = do
    input <- parseInput <$> loadInput
    print $ units input
    print $ leastUnits input
