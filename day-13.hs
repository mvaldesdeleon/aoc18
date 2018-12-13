{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}

import           Control.Applicative
import           Data.Function
import           Data.List
import qualified Data.Map.Strict     as M
import           Data.Maybe

loadInput :: IO String
loadInput = readFile "inputs/day-13.txt"

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

data Cart = Cart
    { cPosition         :: Position
    , cDirection        :: Direction
    , cNextIntersection :: DirectionChange
    } deriving (Show)

cartToChar :: Cart -> Char
cartToChar Cart {..} =
    case cDirection of
        GoUp    -> '^'
        GoDown  -> 'v'
        GoLeft  -> '<'
        GoRight -> '>'

instance Eq Cart where
    (==) = (==) `on` cPosition

instance Ord Cart where
    (<=) = (<=) `on` cPosition

type Tracks = M.Map Position Track

data Mine = Mine
    { mTracks :: Tracks
    , mCarts  :: [Cart]
    , mSize   :: Size
    }

instance Show Mine where
    show Mine {..} = unlines . map row $ [0 .. height - 1]
      where
        Size (width, height) = mSize
        row y = [char $ Position (x, y) | x <- [0 .. width - 1]]
        char p = fromMaybe ' ' (maybeCart p <|> maybeTrack p)
        maybeCart p = cartToChar <$> find ((== p) . cPosition) mCarts
        maybeTrack p = trackToChar <$> M.lookup p mTracks

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

advanceCart :: Mine -> Cart -> Cart
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

main :: IO ()
main = do
    input <- parseInput <$> loadInput
    print $ iterateEither tick input
