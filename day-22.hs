{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

import           Control.Lens
import           Control.Monad.State
import qualified Data.List.Safe      as L
import qualified Data.Map.Strict     as M
import           Data.Maybe          (catMaybes, fromMaybe)
import           Debug.Trace
import           Text.Parsec         (Parsec, char, digit, many1, newline,
                                      parse, string)

tr x = trace (show x) x

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
    } deriving (Show, Eq, Ord)

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

-- best :: Config -> Integer
-- best cfg = fromMaybe 0 $ evalState (b 0 (_target cfg) Torch []) M.empty
--   where
--     l = limit cfg
--     cave = buildCave cfg (Position l l)
--     b :: Integer -> Position -> Tool -> [Position] -> State Best (Maybe Integer)
--     b a p t path
--         | a > l = return Nothing
--         | p == origin && t == Torch = return $ Just 0
--         | p == origin && t /= Torch = return $ Just 7
--         | otherwise = do
--             val <- gets ((p, t) `M.lookup`)
--             case val of
--                 Just v -> return $ Just v
--                 Nothing -> do
--                     let pts = filter (`notElem` path) (neighbors p) >>= addTools
--                     opts <-
--                         catMaybes <$>
--                         forM
--                             pts
--                             (\(p, nt) ->
--                                  ((time t nt +) <$>) <$>
--                                  b (a + time t nt) p nt (p : path))
--                     case L.minimum opts of
--                         Just min -> do
--                             modify (M.insert (tr p, t) min)
--                             return $ Just min
--                         Nothing -> return Nothing
--     valid = within origin (Position l l)
--     neighbors :: Position -> [Position]
--     neighbors p =
--         filter
--             valid
--             [p & x %~ pred, p & y %~ pred, p & x %~ succ, p & y %~ succ]
--     addTools :: Position -> [(Position, Tool)]
--     addTools p = map ((,) p) $ maybe [] tools (p `M.lookup` cave)
--     time :: Tool -> Tool -> Integer
--     time t nt =
--         if t == nt
--             then 1
--             else 8
main :: IO ()
main = do
    input <- parseInput <$> loadInput
    let cave = buildCave' input
    print $ riskLevel cave
    -- print $ limit input
    -- print $ best input
