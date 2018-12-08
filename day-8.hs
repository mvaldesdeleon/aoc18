import           Text.Parsec     (Parsec, count, parse, tokenPrim)
import           Text.Parsec.Pos (incSourceColumn)

loadInput :: IO String
loadInput = readFile "inputs/day-8.txt"

parseInput :: String -> [Integer]
parseInput = map read . words

satisfy f =
    tokenPrim
        show
        (\pos _ _ -> incSourceColumn pos 1)
        (\t ->
             if f t
                 then Just t
                 else Nothing)

value :: Parsec [Integer] () Integer
value = satisfy (const True)

data Tree =
    Node [Tree]
         [Integer]
    deriving (Show)

node :: Parsec [Integer] () Tree
node = do
    c <- value
    m <- value
    Node <$> count (fromIntegral c) node <*> count (fromIntegral m) value

tree :: [Integer] -> Tree
tree input =
    case result of
        Left e  -> error $ show e
        Right r -> r
  where
    result = parse node "" input

main :: IO ()
main = do
    input <- parseInput <$> loadInput
    print $ tree input
