import Text.Parsec
import Data.Functor

loadInput :: IO String
loadInput = readFile "inputs/day-3.txt"

data Rect = Rect {
    rectId :: Integer,
    rectTop :: Integer,
    rectLeft :: Integer,
    rectWidth :: Integer,
    rectHeight :: Integer
} deriving Show

integer :: Parsec String () Integer
integer = read <$> many digit

idTag :: Parsec String () Integer
idTag = char '#' *> integer

at :: Parsec String () ()
at = string " @ " $> ()

colon :: Parsec String () ()
colon = string ": " $> ()

pair :: Char -> Parsec String () (Integer, Integer)
pair separator = (,) <$> (integer <* char separator) <*> integer

position :: Parsec String () (Integer, Integer)
position = pair ','

size :: Parsec String () (Integer, Integer)
size = pair 'x'

rect :: Parsec String () Rect
rect = mkRect <$> (idTag <* at) <*> (position <* colon) <*> size
       where mkRect id (top, left) (width, height) = Rect id top left width height

test :: Parsec String () a -> String -> Either ParseError a
test p = parse p ""

parseInput :: String -> [Rect]
parseInput input = case result of
                    Left e -> error $ show e
                    Right rects -> rects
                   where rects = many (rect <* optional (char '\n'))
                         result = parse rects "" input

main :: IO ()
main = do
    input <- parseInput <$> loadInput
    print $ input
