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
    grid = [[powerLevel gridSerial (x, y) | y <- [1 .. 300]] | x <- [1 .. 300]]
    sumRows = map (scanl (+) 0)
    sumColumns = scanl (zipWith (+)) (replicate 301 0)
    sumGrid = sumColumns . sumRows $ grid
    totalPower z (x, y) =
        ( (sumGrid !! (x + z - 1) !! (y + z - 1)) -
          (sumGrid !! (x - 1) !! (y + z - 1)) -
          (sumGrid !! (x + z - 1) !! (y - 1)) +
          (sumGrid !! (x - 1) !! (y - 1))
        , (fromIntegral x, fromIntegral y, fromIntegral z))

main :: IO ()
main = do
    input <- parseInput <$> loadInput
    print $ maxTotalPower3 input
    print $ maxTotalPower input
