{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TemplateHaskell  #-}

module Day17
    ( day17
    ) where

import           Control.Lens
import           Control.Monad       (replicateM)
import           Control.Monad.Loops (whileM, whileM_)
import           Control.Monad.State (State, execState, runState)
import           Data.Either         (lefts)
import           Data.Function       (on)
import           Data.List           (genericLength, maximumBy, minimumBy,
                                      sortBy)
import qualified Data.Map.Strict     as M
import           Data.Maybe          (fromMaybe)
import           Paths_aoc18         (getDataFileName)
import           Text.Parsec         (Parsec, between, char, digit, many1,
                                      newline, parse, sepBy1, string, (<|>))

loadInput :: IO String
loadInput = getDataFileName "inputs/day-17.txt" >>= readFile

fI :: (Integral a, Num b) => a -> b
fI = fromIntegral

data Range = Range
    { _from :: Integer
    , _to   :: Integer
    } deriving (Show, Eq)

data Position = Position
    { _x :: Integer
    , _y :: Integer
    } deriving (Show, Eq)

makeLenses ''Range

makeLenses ''Position

downwards :: Position -> Position
downwards = over y (+ 1)

leftwards :: Position -> Position
leftwards = over x (subtract 1)

rightwards :: Position -> Position
rightwards = over x (+ 1)

row :: Position -> Position -> [Position]
row l r = [Position x (_y l) | x <- [_x l .. _x r]]

column :: Position -> Position -> [Position]
column t b = [Position (_x t) y | y <- [_y t .. _y b]]

instance Ord Position where
    Position x0 y0 <= Position x1 y1 =
        if y0 /= y1
            then y0 <= y1
            else x0 <= x1

data Rect = Rect
    { _left   :: Integer
    , _top    :: Integer
    , _right  :: Integer
    , _bottom :: Integer
    } deriving (Show, Eq)

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

isClayOrWater :: Ground -> Bool
isClayOrWater = (||) <$> (== Clay) <*> (== Water)

isSandOrWet :: Ground -> Bool
isSandOrWet = (||) <$> (== Sand) <*> (== Wet)

type Slice = M.Map Position Ground

orSand :: Maybe Ground -> Ground
orSand = fromMaybe Sand

lookupOrSand :: Position -> Slice -> Ground
lookupOrSand p s = orSand (p `M.lookup` s)

data Flood = Flood
    { _slice :: Slice
    , _next  :: [Position]
    } deriving (Show)

makeLenses ''Flood

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

spring :: (Position, Ground)
spring = (Position 500 0, Spring)

buildSlice :: [Vein] -> Slice
buildSlice = foldl addVein $ M.fromList [spring]
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
    char p = groundToChar $ p `lookupOrSand` s

flood :: State Flood Integer
flood =
    genericLength <$>
    whileM
        hasNext
        (do sortNext
            next <- popNext
            down <- downwardsSand next
            sideways <- downwardsClayOrWater next
            case (down, sideways) of
                (True, _) -> do
                    bottom <- lastDownwardsEmpty next
                    case bottom of
                        Left p -> wetColumn (downwards next) p
                        Right p -> do
                            nextColumn next p
                            wetColumn (downwards next) p
                (_, True) -> do
                    left <- exploreLeftwards next
                    right <- exploreRightwards next
                    case (left, right) of
                        (Right l, Right r) -> waterRow l r
                        (Right l, Left r)  -> wetRow l r
                        (Left l, Right r)  -> wetRow l r
                        (Left l, Left r)   -> wetRow l r
                    addNext (lefts [left, right])
                _ -> return ())
  where
    hasNext :: State Flood Bool
    hasNext = uses next (not . null)
    sortNext = modifying next (sortBy (flip compare))
    popNext = do
        nxt <- use next
        case nxt of
            (n:ns) -> do
                assign next ns
                return n
    downwardsSand p = uses (slice . at (downwards p)) ((== Sand) . orSand)
    downwardsClayOrWater p =
        uses (slice . at (downwards p)) (isClayOrWater . orSand)
    lastDownwardsEmpty p = uses slice (`lde` p)
    exploreLeftwards p = uses slice (`elw` p)
    exploreRightwards p = uses slice (`erw` p)
    wetColumn t b =
        modifying slice (M.fromList [(p, Wet) | p <- column t b] `M.union`)
    wetRow l r =
        modifying slice (M.fromList [(p, Wet) | p <- row l r] `M.union`)
    waterRow l r =
        modifying slice (M.fromList [(p, Water) | p <- row l r] `M.union`)
    addNext ps = modifying next (++ ps)
    nextColumn t b = addNext (column t b)

lde :: Slice -> Position -> Either Position Position
lde s p
    | _y p > bottom = Left p
    | (== Wet) $ downwards p `lookupOrSand` s = Right p
    | isClayOrWater $ downwards p `lookupOrSand` s = Right p
    | otherwise = lde s (downwards p)
  where
    bottom = _bottom (claySize s)

elw :: Slice -> Position -> Either Position Position
elw s p
    | isSandOrWet $ downwards p `lookupOrSand` s = Left p
    | (== Clay) $ leftwards p `lookupOrSand` s = Right p
    | otherwise = elw s (leftwards p)

erw :: Slice -> Position -> Either Position Position
erw s p
    | isSandOrWet $ downwards p `lookupOrSand` s = Left p
    | (== Clay) $ rightwards p `lookupOrSand` s = Right p
    | otherwise = erw s (rightwards p)

doFlood :: [Vein] -> Slice
doFlood veins = _slice $ execState flood (Flood (buildSlice veins) [fst spring])

waterCount :: [Vein] -> (Integer, Integer)
waterCount vs =
    ( M.foldlWithKey countAll 0 floodedSlice
    , M.foldlWithKey countWater 0 floodedSlice)
  where
    floodedSlice = doFlood vs
    clayRect = claySize floodedSlice
    countAll i pos g =
        if (g == Water || g == Wet || g == Spring) && inRect clayRect pos
            then i + 1
            else i
    countWater i pos g =
        if g == Water
            then i + 1
            else i
    inRect Rect {..} Position {..} = _top <= _y && _y <= _bottom

day17 :: IO ()
day17 = do
    input <- parseInput <$> loadInput
    print $ waterCount input
