import           Data.Char      (digitToInt)
import           Data.Function
import           Data.List
import qualified Data.List.Safe as L
import           Data.Maybe     (fromMaybe)

loadInput :: IO String
loadInput = readFile "inputs/day-11.txt"

parseInput :: String -> Integer
parseInput = read

powerLevel :: Integer -> (Integer, Integer) -> Integer
powerLevel gridSerial (x, y) =
    subtract 5 . hundreds $ (rackId * y + gridSerial) * rackId
  where
    rackId = x + 10

hundreds :: Integer -> Integer
hundreds = fromIntegral . digitToInt . fromMaybe '0' . (L.!! 2) . reverse . show

totalPower :: Integer -> (Integer, Integer) -> Integer
totalPower gridSerial (left, top) =
    sum . map (powerLevel gridSerial) $ positions
  where
    positions = [(x, y) | x <- [left .. left + 2], y <- [top .. top + 2]]

maxTotalPower :: Integer -> (Integer, Integer)
maxTotalPower gridSerial = snd . maximumBy (compare `on` fst) $ options
  where
    options =
        [ (totalPower gridSerial (x, y), (x, y))
        | x <- [1 .. 300]
        , y <- [1 .. 300]
        ]

main :: IO ()
main = do
    input <- parseInput <$> loadInput
    print $ maxTotalPower input
