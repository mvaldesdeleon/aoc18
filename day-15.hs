{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TemplateHaskell       #-}

import           Control.Concurrent  (threadDelay)
import           Control.Lens        (Getting, Sequenced, allOf, filtered,
                                      forMOf_, makeLenses, modifying, toListOf,
                                      views)
import           Control.Monad       (forM, join, replicateM, when)
import           Control.Monad.Loops (untilM, whileM_)
import           Control.Monad.State (MonadState, State, evalState, execState,
                                      get, gets, modify, runState)
import           Data.Char           (intToDigit)
import           Data.Function       (on)
import           Data.List           (find, genericLength, notElem, nub, sort,
                                      sortOn, unionBy)
import qualified Data.List.Safe      as L
import qualified Data.Map.Strict     as M
import           Data.Maybe          (fromMaybe, mapMaybe)
import           Data.Tuple          (fst, swap)

-- LOAD INPUT
loadInput :: IO String
loadInput = readFile "inputs/day-15d.txt"

-- GENERAL HELPERS
mapSnd :: (a -> b) -> (c, a) -> (c, b)
mapSnd f (c, a) = (c, f a)

-- TYPES AND RELATED FUNCTIONS
-- ELEMENTAL TYPES
newtype Size =
    Size (Integer, Integer)
    deriving (Eq, Show)

newtype Position =
    Position (Integer, Integer)
    deriving (Eq, Show)

instance Ord Position where
    Position (x0, y0) <= Position (x1, y1) =
        if y0 /= y1
            then y0 <= y1
            else x0 <= x1

adjacents :: Position -> [Position]
adjacents p = [move GoUp, move GoLeft, move GoRight, move GoDown] <*> [p]

adjacentsD :: Position -> [(Position, Direction)]
adjacentsD p =
    zip ([move GoUp, move GoLeft, move GoRight, move GoDown] <*> [p])
        [GoUp, GoLeft, GoRight, GoDown]

data Direction
    = GoUp
    | GoLeft
    | GoRight
    | GoDown
    deriving (Show, Eq, Ord)

directionToChar :: Direction -> Char
directionToChar =
    \case
        GoUp -> '^'
        GoLeft -> '<'
        GoRight -> '>'
        GoDown -> 'v'

move :: Direction -> Position -> Position
move d (Position (x, y)) =
    case d of
        GoUp    -> Position (x, y - 1)
        GoLeft  -> Position (x - 1, y)
        GoRight -> Position (x + 1, y)
        GoDown  -> Position (x, y + 1)

-- CAVE TILE
data CaveTile
    = Empty
    | Wall
    deriving (Show, Eq)

charToCaveTile :: Char -> CaveTile
charToCaveTile =
    \case
        '#' -> Wall
        '.' -> Empty
        'G' -> Empty
        'E' -> Empty

caveTileToChar :: CaveTile -> Char
caveTileToChar =
    \case
        Empty -> '.'
        Wall -> '#'

-- CAVE
type Cave = M.Map Position CaveTile

isEmpty :: Cave -> Position -> Bool
isEmpty c p = M.findWithDefault Wall p c == Empty

-- UNIT TYPE
data UnitType
    = Goblin
    | Elf
    deriving (Show, Eq)

targetOf :: UnitType -> UnitType
targetOf =
    \case
        Goblin -> Elf
        Elf -> Goblin

-- UNIT
data Unit = Unit
    { _unitId    :: String
    , _unitType  :: UnitType
    , _pos       :: Position
    , _hitPoints :: Integer
    }

instance Show Unit where
    show u =
        unitToChar u :
        " (" ++ show x ++ "," ++ show y ++ ") " ++ show (_hitPoints u)
      where
        Position (x, y) = _pos u

instance Eq Unit where
    (==) = (==) `on` _unitId

instance Ord Unit where
    ua <= ub =
        if _hitPoints ua /= _hitPoints ub
            then _hitPoints ua <= _hitPoints ub
            else _pos ua <= _pos ub

isAlive :: Unit -> Bool
isAlive Unit {..} = _hitPoints > 0

unitToChar :: Unit -> Char
unitToChar Unit {..} =
    case _unitType of
        Goblin -> 'G'
        Elf    -> 'E'

isOfType :: UnitType -> Unit -> Bool
isOfType t Unit {..} = _unitType == t

-- Turns Units into Walls and adds them to the existing Cave
-- Good for calculating range and movement
medusa :: [Unit] -> Cave -> Cave
medusa us = M.union $ M.fromList [(_pos u, Wall) | u <- us]

-- CONFIG
data Config = Config
    { _cave  :: Cave
    , _units :: [Unit]
    , _size  :: Size
    }

instance Show Config where
    show Config {..} = unlines . map row $ [0 .. height - 1]
      where
        Size (width, height) = _size
        row y = [char $ Position (x, y) | x <- [0 .. width - 1]]
        char p = fromMaybe (caveTileAt p) (unitAt p)
        unitAt p = unitToChar <$> find ((== p) . _pos) (filter isAlive _units)
        caveTileAt p = caveTileToChar (_cave M.! p)

-- FLOOD FILL
data FFState = FFState
    { ffNext   :: [(Position, FFResult)]
    , ffResult :: FloodFill
    } deriving (Show)

data FFResult = FFResult
    { distance           :: Integer
    , firstStepDirection :: Direction
    } deriving (Show, Eq, Ord)

type FloodFill = M.Map Position FFResult

floodFill :: Cave -> Position -> FloodFill
floodFill c p =
    ffResult $
    execState
        (do initNext
            -- replicateM_ 5 floodFillStep)
            whileM_ hasNext floodFillStep)
        (FFState [] M.empty)
  where
    initNext :: State FFState ()
    initNext = do
        let initials =
                map (mapSnd (FFResult 1)) . filter (isEmpty c . fst) $
                adjacentsD p
        modify (\ffs@FFState {..} -> ffs {ffNext = initials})
    hasNext :: State FFState Bool
    hasNext = gets ((> 0) . genericLength . ffNext)
    floodFillStep :: State FFState ()
    floodFillStep = do
        ((p, res):rest) <- gets ffNext -- pop next
        modify (\ffs@FFState {..} -> ffs {ffNext = rest})
        modify -- update result
            (\ffs@FFState {..} -> ffs {ffResult = M.insert p res ffResult})
        -- add new to stack
        ff <- gets ffResult
        let new =
                zip
                    (filter (not . visited ff) . filter (isEmpty c) $
                     adjacents p)
                    (repeat $ next res)
        modify
            (\ffs@FFState {..} ->
                 ffs {ffNext = unionBy ((==) `on` fst) ffNext new})
      where
        next ffr@FFResult {..} = ffr {distance = distance + 1}
        visited ff p = p `M.member` ff

-- LENSES
makeLenses ''Unit

makeLenses ''Config

forEach_ :: MonadState s m => Getting (Sequenced r m) s a -> (a -> m r) -> m ()
forEach_ gs f = get >>= \s -> forMOf_ gs s f

-- SPECIFIC CODE
parseCave :: Size -> String -> Cave
parseCave size = M.fromList . map (mapSnd charToCaveTile) . addPosition size

parseUnits :: Size -> String -> [Unit]
parseUnits size = mapMaybe pairToUnit . addPosition size
  where
    pairToUnit (p, c) =
        case c of
            'G' -> Just $ Unit (show p) Goblin p 200
            'E' -> Just $ Unit (show p) Elf p 200
            _   -> Nothing

addPosition :: Size -> String -> [(Position, Char)]
addPosition (Size (width, height)) =
    zip [Position (x, y) | y <- [0 .. height - 1], x <- [0 .. width - 1]]

parseInput :: String -> Config
parseInput input = Config (parseCave size clean) (parseUnits size clean) size
  where
    size = Size (width, height)
    width = genericLength . (!! 0) . lines $ input
    height = genericLength . lines $ input
    clean = filter (/= '\n') input

combatRound :: State Config Config
combatRound = do
    sortUnits
    forEach_ (units . traverse) unitTurn
    get
  where
    sortUnits = modifying units (sortOn _pos)

unitTurn :: Unit -> State Config ()
unitTurn unit =
    when (isAlive unit) $ do
        unitMove unit
        unitAttack unit

unitMove :: Unit -> State Config ()
unitMove Unit {..} = do
    targets <- getTargets _unitType
    positions <- nub . sort . join <$> targets `forM` rangePositions _pos
    when (_pos `notElem` positions) $ do
        ff <- measureFrom _pos
        case L.head . sort . mapMaybe (`M.lookup` ff) $ positions of
            Just FFResult {..} -> moveUnit _unitId firstStepDirection
            Nothing            -> return ()
  where
    getTargets :: UnitType -> State Config [Unit]
    getTargets t =
        gets $
        toListOf
            (units .
             traverse . filtered ((&&) <$> isOfType (targetOf t) <*> isAlive))
    rangePositions :: Position -> Unit -> State Config [Position]
    rangePositions p Unit {..} = do
        cave <- gets (\Config {..} -> medusa (filter isAlive _units) _cave)
        -- Make sure that the current position is considered Empty
        -- Eventually we should adjust the `when` condition
        return $ filter (isEmpty $ M.insert p Empty cave) (adjacents _pos)
    measureFrom :: Position -> State Config FloodFill
    measureFrom p = do
        cave <- gets (\Config {..} -> medusa (filter isAlive _units) _cave)
        return $ floodFill cave p
    moveUnit :: String -> Direction -> State Config ()
    moveUnit id d =
        modifying
            (units . traverse . filtered (views unitId (== id)) . pos)
            (move d)

unitAttack :: Unit -> State Config ()
unitAttack u = do
    units <-
        gets $
        toListOf
            (units .
             traverse .
             filtered
                 ((&&) <$> (isOfType . targetOf . _unitType $ u) <*> isAlive))
    case L.head . sort . mapMaybe (unitAt units) . adjacents $ _pos u of
        Just Unit {..} -> takeDamage _unitId
        Nothing        -> return ()
  where
    unitAt :: [Unit] -> Position -> Maybe Unit
    unitAt us p = find ((==) p . _pos) us
    takeDamage :: String -> State Config ()
    takeDamage id =
        modifying
            (units . traverse . filtered (views unitId (== id)) . hitPoints)
            (subtract 3)

combatOver :: State Config Bool
combatOver = (||) <$> allDead Goblin <*> allDead Elf
  where
    allDead :: UnitType -> State Config Bool
    allDead t =
        gets $ allOf (units . traverse . filtered (isOfType t)) (not . isAlive)

simulate :: Config -> (Config, Integer)
simulate = swap . runState (genericLength <$> (combatRound `untilM` combatOver))

scoreCombat :: (Config, Integer) -> Integer
scoreCombat (cfg, rounds) = rounds * totalHitPoints cfg
  where
    totalHitPoints = sum . toListOf (units . traverse . hitPoints)

outcome :: Config -> Integer
outcome = scoreCombat . simulate

testRounds :: Integer -> Config -> [Config]
testRounds n = evalState (replicateM (fromIntegral n) combatRound)

render :: FloodFill -> Config -> String
render ff Config {..} = unlines . map row $ [0 .. height - 1]
  where
    Size (width, height) = _size
    row y = [char $ Position (x, y) | x <- [0 .. width - 1]]
    char p = fromMaybe (caveTileAt p) (unitAt p)
    unitAt p = unitToChar <$> find ((== p) . _pos) (filter isAlive _units)
    caveTileAt p =
        if isEmpty _cave p
            then dirAt p
            else caveTileToChar (_cave M.! p)
    dirAt p = maybe '.' directionToChar (firstStepDirection <$> p `M.lookup` ff)
    distAt p =
        if dist < 16
            then intToDigit dist
            else '-'
      where
        dist = fromIntegral . distance $ ff M.! p

frame :: Config -> IO ()
frame c = do
    putStrLn $ show c
    -- threadDelay 300

main :: IO ()
main = do
    input <- parseInput <$> loadInput
    -- let cave = _cave input
    -- let units = _units input
    -- let pos = _pos $ _units input !! 0
    -- let ff = floodFill (medusa units cave) pos
    -- putStrLn $ render ff input
    -- print $ testRounds 5 input
    print $ outcome input
    let i = 50
    let combat = testRounds (fromIntegral i + 1) input
    -- frame `mapM_` combat
    print $ combat !! i
    print $ sortOn _pos . _units $ combat !! i
