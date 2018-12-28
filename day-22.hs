{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

import           Control.Lens
import           Control.Monad.Loops (whileM_)
import           Control.Monad.State
import           Data.List           (intersect)
import qualified Data.List.Safe      as L
import qualified Data.Map.Strict     as M
import           Data.Maybe          (catMaybes, fromMaybe)
import           Text.Parsec         (Parsec, char, digit, many1, newline,
                                      parse, string)

loadInput :: IO String
loadInput = readFile "inputs/day-22.txt"

data Region
    = Rocky
    | Wet
    | Narrow
    deriving (Show, Eq, Ord, Enum)

data Tool
    = Torch
    | ClimbingGear
    | Neither
    deriving (Show, Eq, Ord)

tools :: Region -> [Tool]
tools =
    \case
        Rocky -> [Torch, ClimbingGear]
        Wet -> [ClimbingGear, Neither]
        Narrow -> [Torch, Neither]

data Position = Position
    { _x :: Integer
    , _y :: Integer
    } deriving (Show, Eq)

instance Ord Position where
    Position xa ya <= Position xb yb =
        if ya /= yb
            then ya <= yb
            else xa <= xb

makeLenses ''Position

origin :: Position
origin = Position 0 0

range :: Position -> Position -> [Position]
range (Position left top) (Position right bottom) =
    [Position x y | y <- [top .. bottom], x <- [left .. right]]

within :: Position -> Position -> Position -> Bool
within (Position left top) (Position right bottom) (Position x y) =
    left <= x && x <= right && top <= y && y <= bottom

data Config = Config
    { _depth  :: Integer
    , _target :: Position
    } deriving (Show, Eq)

makeLenses ''Config

fI :: (Integral a, Num b) => a -> b
fI = fromIntegral

number :: Parsec String () Integer
number = read <$> many1 digit

parseDepth :: Parsec String () Integer
parseDepth = string "depth: " *> number

parseTarget :: Parsec String () Position
parseTarget =
    Position <$> (string "target: " *> number) <*> (char ',' *> number)

parseConfig :: Parsec String () Config
parseConfig = Config <$> (parseDepth <* newline) <*> parseTarget

parseInput :: String -> Config
parseInput input =
    case res of
        Left e  -> error $ show e
        Right c -> c
  where
    res = parse parseConfig "" input

type Cave = M.Map Position Region

buildCave :: Config -> Position -> Cave
buildCave Config {..} bottomRight =
    fst $ foldl f (M.empty, M.empty) (range origin bottomRight)
  where
    f (cave, gis) p =
        let gi = geologicIndex gis p
         in ( M.insert p (toEnum . fI $ erosionLevel gi `mod` 3) cave
            , M.insert p gi gis)
    geologicIndex gis p@Position {..}
        | p == origin || p == _target = 0
        | _y == 0 = _x * 16807
        | _x == 0 = _y * 48271
        | otherwise =
            case ( Position (_x - 1) _y `M.lookup` gis
                 , Position _x (_y - 1) `M.lookup` gis) of
                (Just gia, Just gib) -> erosionLevel gia * erosionLevel gib
                _ -> error "geologic index for region missing"
    erosionLevel gi = (gi + _depth) `mod` 20183

buildCave' :: Config -> Cave
buildCave' cfg = buildCave cfg (_target cfg)

riskLevel :: Cave -> Integer
riskLevel = foldl f 0
  where
    f rl = (+ rl) . fI . fromEnum

limit :: Config -> Integer
limit cfg = (cfg ^. target . x + cfg ^. target . y) * 7 `div` 2

type Best = M.Map (Position, Tool) Integer

data BState = BState
    { _best   :: Best
    , _next   :: Best
    , _result :: Integer
    } deriving (Show)

makeLenses ''BState

bestToTarget :: Config -> Integer
bestToTarget cfg =
    _result $
    execState
        buildBest
        (BState M.empty (M.fromList [((origin, Torch), 0)]) bound)
  where
    bound = 1048
    -- bound = trivialCost cfg
    l = limit cfg
    -- bottomRight = _target cfg
    bottomRight = Position (cfg ^. target . y) (cfg ^. target . y)
    cave = buildCave cfg bottomRight
    hasNext :: State BState Bool
    hasNext = uses next (not . M.null)
    popNext :: State BState (Position, Tool, Integer)
    popNext = do
        (((p, t), i), ns) <- uses next M.deleteFindMin
        next .= ns
        return (p, t, i)
    addResult :: Position -> Tool -> Integer -> State BState ()
    addResult p t i = do
        best %= M.insert (p, t) i
        when (p == _target cfg && t == Torch) $ result .= i
        next %=
            M.unionWith
                min
                (M.fromList
                     [ ((p, nt), i + time t nt)
                     | (p, nt) <- neighbors p >>= addTools p
                     ])
    buildBest :: State BState ()
    buildBest =
        whileM_
            hasNext
            (do (p, t, i) <- popNext
                res <- use result
                when (i + minDistanceToTarget p <= res) $ do
                    val <- uses best ((p, t) `M.lookup`)
                    case val of
                        Just v ->
                            if v < i
                                then return ()
                                else addResult p t i
                        Nothing -> addResult p t i)
    valid = within origin bottomRight
    neighbors :: Position -> [Position]
    neighbors p =
        filter
            valid
            [p & x %~ pred, p & x %~ succ, p & y %~ succ, p & y %~ pred]
    addTools :: Position -> Position -> [(Position, Tool)]
    addTools op np = map ((,) np) (ots `intersect` nts)
      where
        ots = maybe [] tools (op `M.lookup` cave)
        nts = maybe [] tools (np `M.lookup` cave)
    time :: Tool -> Tool -> Integer
    time t nt =
        if t == nt
            then 1
            else 8
    minDistanceToTarget :: Position -> Integer
    minDistanceToTarget p =
        abs (cfg ^. target . x - p ^. x) + abs (cfg ^. target . y - p ^. y)

trivialCost :: Config -> Integer
trivialCost cfg = adjust . foldl f (0, Torch) . map (cave M.!) $ path
  where
    targetX = cfg ^. target . x
    targetY = cfg ^. target . y
    cave = buildCave' cfg
    path =
        [Position 0 y | y <- [0 .. targetY]] ++
        [Position x targetY | x <- [0 .. targetX]]
    f (i, t) r = (i + time r t, tool r t)
    tool r t =
        if t `elem` tools r
            then t
            else head $ tools r
    time r t =
        if t == tool r t
            then 1
            else 8
    adjust (i, t) =
        if t == Torch
            then i
            else i + 7

main :: IO ()
main = do
    input <- parseInput <$> loadInput
    let cave = buildCave' input
    print $ riskLevel cave
    -- print $ limit input
    -- print $ trivialCost input
    print $ bestToTarget input
