{-# LANGUAGE RecordWildCards #-}

module Day10
    ( day10
    ) where

import qualified Data.Set    as S
import           Paths_aoc18 (getDataFileName)
import           Text.Parsec

loadInput :: IO String
loadInput = getDataFileName "inputs/day-10.txt" >>= readFile

data Light = Light
    { _x  :: Integer
    , _y  :: Integer
    , _vx :: Integer
    , _vy :: Integer
    } deriving (Show)

number :: Parsec String () Integer
number = read <$> (many space *> rawNumber)
  where
    rawNumber = (:) <$> option ' ' (char '-') <*> many1 digit

pair :: Parsec String () (Integer, Integer)
pair = (,) <$> (char '<' *> number) <*> (char ',' *> number <* char '>')

light :: Parsec String () Light
light =
    mkLight <$> (string "position=" *> pair) <*>
    (space *> string "velocity=" *> pair) <*
    optional newline
  where
    mkLight (x, y) (vx, vy) = Light x y vx vy

parseInput :: String -> [Light]
parseInput input =
    case result of
        Left e  -> error $ show e
        Right r -> r
  where
    result = parse (many light) "" input

advance :: Integer -> Light -> Light
advance t Light {..} = Light (_x + t * _vx) (_y + t * _vy) _vx _vy

center :: [Light] -> Light
center ls =
    Light (_x `div` size) (_y `div` size) (_vx `div` size) (_vy `div` size)
  where
    size = fromIntegral . length $ ls
    Light {..} = foldl sum (Light 0 0 0 0) ls
    sum Light {..} (Light x y vx vy) =
        Light (x + _x) (y + _y) (vx + _vx) (vy + _vy)

spread :: [Light] -> Integer
spread ls = sum . map (distance c) $ ls
  where
    c = center ls

distance :: Light -> Light -> Integer
distance Light {..} (Light x y _ _) =
    round . sqrt . fromIntegral $ (dx * dx + dy * dy)
  where
    dx = x - _x
    dy = y - _y

localMin :: Ord a => [a] -> Integer
localMin = _localMin 0
  where
    _localMin n (a1:a2:a3:as) =
        if a1 > a2 && a2 < a3
            then n + 1
            else _localMin (n + 1) (a2 : a3 : as)

data Box = Box
    { _top    :: Integer
    , _left   :: Integer
    , _bottom :: Integer
    , _right  :: Integer
    } deriving (Show)

boundingBox :: [Light] -> Box
boundingBox = foldl bounding (Box 0 100000 100000 0)
  where
    bounding Box {..} Light {..} =
        Box (max _top _y) (min _left _x) (min _bottom _y) (max _right _x)

spreads :: [Light] -> [Integer]
spreads ls = map spread sequence
  where
    time = [0 ..]
    sequence = map (`advanceAll` ls) time

advanceAll :: Integer -> [Light] -> [Light]
advanceAll t = map (advance t)

render :: [Light] -> String
render ls =
    unlines [[charAt (x, y) | x <- [_left .. _right]] | y <- [_bottom .. _top]]
  where
    Box {..} = boundingBox ls
    charAt pos =
        if S.member pos set
            then '#'
            else '.'
    set = foldl addPos S.empty ls
    addPos s Light {..} = S.insert (_x, _y) s

day10 :: IO ()
day10 = do
    input <- parseInput <$> loadInput
    let instant = localMin (spreads input)
    print $ instant
    putStrLn $ render (advanceAll instant input)
