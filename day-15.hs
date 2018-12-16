{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TemplateHaskell       #-}

import           Control.Lens        (Getting, Sequenced, allOf, filtered,
                                      forMOf_, makeLenses, modifying, toListOf)
import           Control.Monad       (forM_, when)
import           Control.Monad.Loops (untilM)
import           Control.Monad.State (MonadState, State, get, gets, runState)
import           Data.List           (find, genericLength, sortOn)
import qualified Data.Map.Strict     as M
import           Data.Maybe          (fromMaybe, mapMaybe)
import           Data.Tuple          (swap)

-- LOAD INPUT
loadInput :: IO String
loadInput = readFile "inputs/day-15.txt"

-- TYPES AND RELATED FUNCTIONS
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

type Cave = M.Map Position CaveTile

data UnitType
    = Goblin
    | Elf
    deriving (Show, Eq)

data Unit = Unit
    { _unitType  :: UnitType
    , _pos       :: Position
    , _hitPoints :: Integer
    } deriving (Show)

isAlive :: Unit -> Bool
isAlive Unit {..} = _hitPoints > 0

unitToChar :: Unit -> Char
unitToChar Unit {..} =
    case _unitType of
        Goblin -> 'G'
        Elf    -> 'E'

isOfType :: UnitType -> Unit -> Bool
isOfType t Unit {..} = _unitType == t

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

-- LENSES
makeLenses ''Unit

makeLenses ''Config

forEach_ :: MonadState s m => Getting (Sequenced r m) s a -> (a -> m r) -> m ()
forEach_ gs f = get >>= \s -> forMOf_ gs s f

-- GENERAL GELPERS
mapSnd :: (a -> b) -> (c, a) -> (c, b)
mapSnd f (c, a) = (c, f a)

-- SPECIFIC CODE
parseCave :: Size -> String -> Cave
parseCave size = M.fromList . map (mapSnd charToCaveTile) . addPosition size

parseUnits :: Size -> String -> [Unit]
parseUnits size = mapMaybe pairToUnit . addPosition size
  where
    pairToUnit (p, c) =
        case c of
            'G' -> Just $ Unit Goblin p 200
            'E' -> Just $ Unit Elf p 200
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

combatRound :: State Config ()
combatRound = do
    sortUnits
    forEach_ (units . traverse) unitTurn
  where
    sortUnits = modifying units (sortOn _pos)

unitTurn :: Unit -> State Config ()
unitTurn unit =
    when (isAlive unit) $ do
        unitMove unit
        unitAttack unit

unitMove :: Unit -> State Config ()
-- identify targets
-- identify all open squares in range (OSIR) of target
-- check if already in range of target
-- flood-fill measuring distance from position of unit to all OSIRs
--    each square hold a Maybe (distance, directionFirtStep)
--      when a square is reached multiple times we keep the best distance
--      and if the distance is tied, the best directionFirstStep by reading order
--    we also need to keep the backtracking matrix...
--       actually, all we really need to know is the FIRST step taken,
--       so theres no need to actually backtrack, just propagate this value
--    use comibined map with positions of all alive units
-- remove all values with no distance (unreachable)
-- sort them by distance and then by position (reading order)
-- pick the first, if any and take the step indicated
unitMove = undefined

unitAttack :: Unit -> State Config ()
-- identify targets in range
-- sort by HP and then by position (reading order)
-- pick the first, if any and reduce its HP by 3
unitAttack = undefined

combatOver :: State Config Bool
combatOver = (||) <$> allDead Goblin <*> allDead Elf
  where
    allDead :: UnitType -> State Config Bool
    -- maybe refactor to "uses" later on ?
    allDead t =
        gets $ allOf (units . traverse . filtered (isOfType t)) (not . isAlive)

simulate :: Config -> (Config, Integer)
simulate = swap . runState (genericLength <$> (combatRound `untilM` combatOver))

scoreCombat :: (Config, Integer) -> Integer
scoreCombat (cfg, rounds) = rounds * totalHP cfg
  where
    totalHP = sum . toListOf (units . traverse . hitPoints)

outcome :: Config -> Integer
outcome = scoreCombat . simulate

main :: IO ()
main = do
    input <- parseInput <$> loadInput
    print $ outcome input
