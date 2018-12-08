import qualified Data.List.Safe  as L
import           Data.Maybe      (mapMaybe)
import           Text.Parsec     (Parsec, count, parse, tokenPrim)
import           Text.Parsec.Pos (incSourceColumn)

loadInput :: IO String
loadInput = readFile "inputs/day-8.txt"

parseInput :: String -> Tree
parseInput = tree . map read . words

satisfy f =
    tokenPrim
        show
        (\pos _ _ -> incSourceColumn pos 1)
        (\t ->
             if f t
                 then Just t
                 else Nothing)

num :: Parsec [Integer] () Integer
num = satisfy (const True)

data Tree =
    Node [Tree]
         [Integer]
    deriving (Show)

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
metadata (Node cs ms) = sum ms + (sum . map metadata $ cs)

value :: Tree -> Integer
value (Node cs ms) =
    if null cs
        then sum ms
        else sum . map value $ indexedChildren cs ms
  where
    indexedChildren cs = mapMaybe ((L.!!) cs . pred)

main :: IO ()
main = do
    input <- parseInput <$> loadInput
    print $ metadata input
    print $ value input
