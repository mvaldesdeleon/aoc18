{-# LANGUAGE RecordWildCards #-}

import           Data.Function
import           Data.Functor
import           Data.List
import           Text.Parsec

loadInput :: IO String
loadInput = readFile "inputs/day-3.txt"

data Rect = Rect
    { rectId     :: Integer
    , rectTop    :: Integer
    , rectLeft   :: Integer
    , rectWidth  :: Integer
    , rectHeight :: Integer
    } deriving (Show)

-- Parsing stuff
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
  where
    mkRect id (left, top) (width, height) = Rect id top left width height

test :: Parsec String () a -> String -> Either ParseError a
test p = parse p ""

parseInput :: String -> [Rect]
parseInput input =
    case result of
        Left e      -> error $ show e
        Right rects -> rects
  where
    rects = many (rect <* optional (char '\n'))
    result = parse rects "" input

-- Actual problem
overlappedArea :: [Rect] -> Integer
overlappedArea = sum . partialAreas

rectYs :: Rect -> [Integer]
rectYs Rect {..} = [rectTop, rectTop + rectHeight]

partialAreas :: [Rect] -> [Integer]
partialAreas rects = sectionAreas ys sorted
  where
    ys = nub . sort $ rects >>= rectYs
    sorted = sortBy (compare `on` rectLeft) rects

sectionAreas :: [Integer] -> [Rect] -> [Integer]
sectionAreas (from:to:ys) rects =
    sectionArea from to rects : sectionAreas (to : ys) rects
sectionAreas _ _ = []

sectionArea :: Integer -> Integer -> [Rect] -> Integer
sectionArea from to rects = height * sum (overlaps sectionRects)
  where
    height = to - from
    inSection from to Rect {..} = from < (rectTop + rectHeight) && rectTop < to
    sectionRects = filter (inSection from to) rects

overlaps :: [Rect] -> [Integer]
overlaps rects = overlappingIntervals (tail . scanl sum empty $ borders)
  where
    toBorders Rect {..} = [(rectLeft, 1), (rectLeft + rectWidth, -1)]
    borders = sortBy (compare `on` fst) $ rects >>= toBorders
    sum (_, n) (x, m) = (x, n + m)
    empty = (0, 0)

depthIntervals :: [(Integer, Integer)] -> [(Integer, Integer)]
depthIntervals ((from, fromDepth):(to, toDepth):bs) =
    (to - from, fromDepth) : depthIntervals ((to, toDepth) : bs)
depthIntervals _ = []

overlappingIntervals :: [(Integer, Integer)] -> [Integer]
overlappingIntervals =
    map fst . filter (\(_, depth) -> depth > 1) . depthIntervals

overlap :: Rect -> Rect -> Bool
overlap a b =
    (rectId a /= rectId b) &&
    (rectBottom b > rectTop a) &&
    (rectTop b < rectBottom a) &&
    (rectRight b > rectLeft a) && (rectLeft b < rectRight a)
  where
    rectBottom r = rectTop r + rectHeight r
    rectRight r = rectLeft r + rectWidth r

nonOverlappingId :: [Rect] -> Integer
nonOverlappingId rects =
    rectId . head $ [r | r <- rects, all (not . overlap r) rects]

main :: IO ()
main = do
    input <- parseInput <$> loadInput
    print $ overlappedArea input
    print $ nonOverlappingId input
