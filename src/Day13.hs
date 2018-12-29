{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}

module Day13
    ( day13
    ) where

import           Control.Applicative ((<|>))
import           Control.Concurrent  (threadDelay)
import           Data.Function       (on)
import           Data.List           (elem, elemIndex, find, genericLength,
                                      sort)
import qualified Data.Map.Strict     as M
import           Data.Maybe          (fromMaybe, mapMaybe)
import           Paths_aoc18         (getDataFileName)

loadInput :: IO String
loadInput = getDataFileName "inputs/day-13.txt" >>= readFile

newtype Position =
    Position (Integer, Integer)
    deriving (Eq, Show)

newtype Size =
    Size (Integer, Integer)
    deriving (Eq, Show)

instance Ord Position where
    Position (x0, y0) <= Position (x1, y1) =
        if y0 /= y1
            then y0 <= y1
            else x0 <= x1

data Track
    = Horizontal
    | Vertical
    | Intersection
    | UpLeft
    | UpRight
    deriving (Show, Eq)

charToTrack :: Char -> Maybe Track
charToTrack =
    \case
        '-' -> Just Horizontal
        '<' -> Just Horizontal
        '>' -> Just Horizontal
        '|' -> Just Vertical
        '^' -> Just Vertical
        'v' -> Just Vertical
        '+' -> Just Intersection
        '\\' -> Just UpLeft
        '/' -> Just UpRight
        _ -> Nothing

trackToChar :: Track -> Char
trackToChar =
    \case
        Horizontal -> '-'
        Vertical -> '|'
        Intersection -> '+'
        UpLeft -> '\\'
        UpRight -> '/'

data Direction
    = GoUp
    | GoDown
    | GoLeft
    | GoRight
    deriving (Show, Eq)

turn :: Track -> Direction -> Direction
turn t d =
    case t of
        UpLeft ->
            case d of
                GoUp    -> GoLeft
                GoDown  -> GoRight
                GoLeft  -> GoUp
                GoRight -> GoDown
        UpRight ->
            case d of
                GoUp    -> GoRight
                GoDown  -> GoLeft
                GoLeft  -> GoDown
                GoRight -> GoUp
        _ -> d

data DirectionChange
    = CounterClockWise
    | NoChange
    | ClockWise
    deriving (Show)

next :: DirectionChange -> DirectionChange
next =
    \case
        CounterClockWise -> NoChange
        NoChange -> ClockWise
        ClockWise -> CounterClockWise

change :: DirectionChange -> Direction -> Direction
change dc d =
    case dc of
        CounterClockWise -> ccw d
        NoChange         -> d
        ClockWise        -> cw d
  where
    ccw =
        \case
            GoUp -> GoLeft
            GoDown -> GoRight
            GoLeft -> GoDown
            GoRight -> GoUp
    cw =
        \case
            GoUp -> GoRight
            GoDown -> GoLeft
            GoLeft -> GoUp
            GoRight -> GoDown

data Cart
    = Removed
    | Cart { cPosition         :: Position
           , cDirection        :: Direction
           , cNextIntersection :: DirectionChange }
    deriving (Show)

isRemoved :: Cart -> Bool
isRemoved =
    \case
        Removed -> True
        _ -> False

cartToChar :: Cart -> Char
cartToChar Cart {..} =
    case cDirection of
        GoUp    -> '^'
        GoDown  -> 'v'
        GoLeft  -> '<'
        GoRight -> '>'

instance Eq Cart where
    Cart pa _ _ == Cart pb _ _ = pa == pb
    _ == _ = False

instance Ord Cart where
    Cart pa _ _ <= Cart pb _ _ = pa <= pb
    Cart {} <= Removed = False
    _ <= _ = True

type Tracks = M.Map Position Track

data Mine = Mine
    { mTracks :: Tracks
    , mCarts  :: [Cart]
    , mSize   :: Size
    }

render :: Position -> Size -> Mine -> String
render (Position (left, top)) (Size (width, height)) Mine {..} =
    unlines . map row $ [top .. top + height - 1]
  where
    row y = [char $ Position (x, y) | x <- [left .. left + width - 1]]
    char p = fromMaybe ' ' (maybeCart p <|> maybeTrack p)
    maybeCart p =
        cartToChar <$>
        find ((== p) . cPosition) (filter (not . isRemoved) mCarts)
    maybeTrack p = trackToChar <$> M.lookup p mTracks

instance Show Mine where
    show m@Mine {..} = render (Position (0, 0)) mSize m

mapSnd :: (a -> b) -> (c, a) -> (c, b)
mapSnd f (c, a) = (c, f a)

parseTracks :: Size -> String -> Tracks
parseTracks size =
    M.fromList . mapMaybe (sequence . mapSnd charToTrack) . addPosition size

parseCarts :: Size -> String -> [Cart]
parseCarts size = mapMaybe pairToCart . addPosition size
  where
    pairToCart (p, c) =
        case c of
            '^' -> Just $ Cart p GoUp CounterClockWise
            'v' -> Just $ Cart p GoDown CounterClockWise
            '<' -> Just $ Cart p GoLeft CounterClockWise
            '>' -> Just $ Cart p GoRight CounterClockWise
            _   -> Nothing

addPosition :: Size -> String -> [(Position, Char)]
addPosition (Size (width, height)) =
    zip [Position (x, y) | y <- [0 .. height - 1], x <- [0 .. width - 1]]

parseInput :: String -> Mine
parseInput input =
    Mine (parseTracks size clean) (sort $ parseCarts size clean) size
  where
    size = Size (width, height)
    width = genericLength . (!! 0) . lines $ input
    height = genericLength . lines $ input
    clean = filter (/= '\n') input

tick :: Mine -> Either Position Mine
tick m@Mine {..} =
    sortCarts <$> foldl subTick (Right m) [0 .. genericLength mCarts - 1]
  where
    sortCarts m@Mine {..} = m {mCarts = sort mCarts}
    subTick e i =
        case e of
            Left p  -> Left p
            Right m -> doSubTick m i
    doSubTick m@Mine {..} i =
        if collision
            then Left $ cPosition newCart
            else Right $ updateCart m i newCart
      where
        cart = mCarts !! fromIntegral i
        newCart = advanceCart m cart
        collision = newCart `elem` mCarts
    updateCart m@Mine {..} i c = m {mCarts = replace i c mCarts}

tick' :: Mine -> Either Position Mine
tick' m@Mine {..} =
    sortCarts <$> foldl subTick (Right m) [0 .. genericLength mCarts - 1]
  where
    sortCarts m@Mine {..} = m {mCarts = sort mCarts}
    subTick e i =
        case e of
            Left p  -> Left p
            Right m -> doSubTick m i
    doSubTick m@Mine {..} i
        | lastCart = Left $ cPosition (head . filter (not . isRemoved) $ mCarts)
        | isRemoved cart = Right m
        | collision = Right $ removeCarts m i j
        | otherwise = Right $ updateCart m i newCart
      where
        lastCart = (==) 1 . length . filter (not . isRemoved) $ mCarts
        cart = mCarts !! fromIntegral i
        newCart = advanceCart m cart
        collision = newCart `elem` mCarts
        j =
            maybe
                (error "This will never happen")
                fromIntegral
                (newCart `elemIndex` mCarts)
    updateCart m@Mine {..} i c = m {mCarts = replace i c mCarts}
    removeCarts m@Mine {..} i j =
        m {mCarts = replace i Removed . replace j Removed $ mCarts}

advanceCart :: Mine -> Cart -> Cart
advanceCart _ Removed = Removed
advanceCart Mine {..} c@Cart {..} =
    c
        { cPosition = newPosition
        , cDirection = newDirection
        , cNextIntersection = newNextIntersection
        }
  where
    newPosition = move cDirection cPosition
    trackAhead = mTracks M.! newPosition
    newDirection =
        case trackAhead of
            Intersection -> change cNextIntersection cDirection
            UpLeft       -> turn trackAhead cDirection
            UpRight      -> turn trackAhead cDirection
            _            -> cDirection
    newNextIntersection =
        case trackAhead of
            Intersection -> next cNextIntersection
            _            -> cNextIntersection

move :: Direction -> Position -> Position
move d (Position (x, y)) =
    case d of
        GoUp    -> Position (x, y - 1)
        GoDown  -> Position (x, y + 1)
        GoLeft  -> Position (x - 1, y)
        GoRight -> Position (x + 1, y)

replace :: Integer -> a -> [a] -> [a]
replace i v (a:as) =
    if i == 0
        then v : as
        else a : replace (i - 1) v as
replace _ _ [] = []

iterateEither :: (b -> Either a b) -> b -> a
iterateEither f b =
    case f b of
        Left a  -> a
        Right b -> iterateEither f b

itergregateEither :: (b -> Either a b) -> b -> [b]
itergregateEither f b = go f b []
  where
    go f b res =
        case f b of
            Left a   -> reverse (b : res)
            Right nb -> go f nb (b : res)

frame :: Mine -> IO ()
frame m = do
    putStrLn $ render (Position (00, 00)) (Size (150, 150)) m
    threadDelay 300000

day13 :: IO ()
day13 = do
    input <- parseInput <$> loadInput
    -- frame `mapM_` itergregateEither tick input
    print $ iterateEither tick input
    print $ iterateEither tick' input
