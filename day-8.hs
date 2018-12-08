import qualified Data.List.Safe as L
import           Data.Maybe     (mapMaybe)
import           Text.Parsec    (Parsec, anyToken, count, parse)

loadInput :: IO String
loadInput = readFile "inputs/day-8.txt"

parseInput :: String -> Tree
parseInput = tree . map read . words

num :: Parsec [Integer] () Integer
num = anyToken

data Tree =
    Node [Tree]
         [Integer]
    deriving (Show)

foldTree :: ([Integer] -> [b] -> b) -> Tree -> b
foldTree f (Node cs ms) = f ms (foldTree f <$> cs)

node :: Parsec [Integer] () Tree
node = do
    c <- num
    m <- num
    Node <$> count (fromIntegral c) node <*> count (fromIntegral m) num

tree :: [Integer] -> Tree
tree input =
    case result of
        Left e  -> error $ show e
        Right r -> r
  where
    result = parse node "" input

metadata :: Tree -> Integer
metadata = foldTree f
  where
    f ms sums = sum ms + sum sums

value :: Tree -> Integer
value = foldTree f
  where
    f ms values =
        if null values
            then sum ms
            else sum . mapMaybe ((L.!!) values . pred) $ ms

main :: IO ()
main = do
    input <- parseInput <$> loadInput
    print $ metadata input
    print $ value input
