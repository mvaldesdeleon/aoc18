module Day5
    ( day5
    ) where

import           Data.Char
import           Data.List.Zipper
import           Paths_aoc18      (getDataFileName)

loadInput :: IO String
loadInput = getDataFileName "inputs/day-5.txt" >>= readFile

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
                else reduceRight $ right z
        _ -> z
  where
    nz = right z
    mv = safeCursor z
    mn = safeCursor nz

reduceLeft :: Zipper Char -> Zipper Char
reduceLeft z =
    if beginp z
        then reduceRight z
        else case (mv, mp) of
                 (Just v, Just p) ->
                     if match v p
                         then reduceLeft $ delete . pop $ z
                         else reduceRight z
                 _ -> reduceRight z
  where
    pz = left z
    mv = safeCursor z
    mp = safeCursor pz

match :: Char -> Char -> Bool
match a b = a /= b && toLower a == toLower b

leastUnits :: [Char] -> Integer
leastUnits input = minimum [units $ removeUnit u input | u <- ['a' .. 'z']]

removeUnit :: Char -> [Char] -> [Char]
removeUnit u = filter (toLower u /=) . filter (toUpper u /=)

day5 :: IO ()
day5 = do
    input <- parseInput <$> loadInput
    print $ units input
    print $ leastUnits input
