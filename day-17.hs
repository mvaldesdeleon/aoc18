{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

import           Control.Lens
import           Data.Function   (on)
import           Data.List       (maximumBy, minimumBy)
import qualified Data.Map.Strict as M
import           Data.Maybe      (fromMaybe)
import           Text.Parsec     (Parsec, between, char, digit, many1, newline,
                                  parse, sepBy1, string, (<|>))

loadInput :: IO String
loadInput = readFile "inputs/day-17a.txt"

fI :: (Integral a, Num b) => a -> b
fI = fromIntegral

data Range = Range
    { _from :: Integer
    , _to   :: Integer
    } deriving (Show, Eq, Ord)

data Position = Position
    { _x :: Integer
    , _y :: Integer
    } deriving (Show, Eq, Ord)

data Rect = Rect
    { _left   :: Integer
    , _top    :: Integer
    , _right  :: Integer
    , _bottom :: Integer
    } deriving (Show, Eq, Ord)

makeLenses ''Range

makeLenses ''Position

makeLenses ''Rect

data Vein
    = Horizontal Integer
                 Range
    | Vertical Integer
               Range
    deriving (Show)

data Ground
    = Sand
    | Clay
    | Spring
    | Water
    | Wet
    deriving (Show, Eq)

groundToChar :: Ground -> Char
groundToChar =
    \case
        Sand -> '.'
        Clay -> '#'
        Spring -> '+'
        Water -> '~'
        Wet -> '|'

type Slice = M.Map Position Ground

number :: Parsec String () Integer
number = read <$> many1 digit

parseRange :: Parsec String () Range
parseRange = Range <$> number <*> (string ".." *> number)

parseVein :: Parsec String () Vein
parseVein = parseHorizontal <|> parseVertical
  where
    parseHorizontal = parseVeinAny 'y' 'x' Horizontal
    parseVertical = parseVeinAny 'x' 'y' Vertical
    parseVeinAny f s ctor =
        ctor <$> (char f *> char '=' *> number) <*>
        (string ", " *> char s *> char '=' *> parseRange)

parseInput :: String -> [Vein]
parseInput input =
    case result of
        Left e  -> error $ show e
        Right r -> r
  where
    result = parse (parseVein `sepBy1` newline) "" input

buildSlice :: [Vein] -> Slice
buildSlice = foldl addVein $ M.fromList [(Position 500 0, Spring)]
  where
    addVein :: Slice -> Vein -> Slice
    addVein s =
        \case
            Horizontal y Range {..} ->
                s `M.union`
                M.fromList [(Position x y, Clay) | x <- [_from .. _to]]
            Vertical x Range {..} ->
                s `M.union`
                M.fromList [(Position x y, Clay) | y <- [_from .. _to]]

claySize :: Slice -> Rect
claySize = size . M.filter (== Clay)

size :: Slice -> Rect
size s = Rect left top right bottom
  where
    ps = M.keys s
    left = _x . minimumBy (compare `on` _x) $ ps
    top = _y . minimumBy (compare `on` _y) $ ps
    right = _x . maximumBy (compare `on` _x) $ ps
    bottom = _y . maximumBy (compare `on` _y) $ ps

render :: Slice -> String
render s = unlines . map row $ [_top .. _bottom]
  where
    Rect {..} = size s
    row y = [char $ Position x y | x <- [_left - 1 .. _right + 1]]
    char p = groundToChar $ fromMaybe Sand (p `M.lookup` s)

main :: IO ()
main = do
    input <- parseInput <$> loadInput
    let slice = buildSlice input
    putStrLn $ render slice
