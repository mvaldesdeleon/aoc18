{-# LANGUAGE FlexibleContexts #-}

module Day6
    ( day6
    ) where

import           Control.Monad.State
import           Data.Function
import           Data.List
import qualified Data.Map.Strict     as M
import           Paths_aoc18         (getDataFileName)
import           Text.Parsec         (Parsec, char, digit, many, optional,
                                      parse, string)

loadInput :: IO String
loadInput = getDataFileName "inputs/day-6.txt" >>= readFile

newtype Coord =
    Coord (Integer, Integer)
    deriving (Show)

distance :: Coord -> Coord -> Integer
distance a b = abs (getX a - getX b) + abs (getY a - getY b)

getX :: Coord -> Integer
getX (Coord (x, _)) = x

getY :: Coord -> Integer
getY (Coord (_, y)) = y

-- Parsing stuff
integer :: Parsec String () Integer
integer = read <$> many digit

coord :: Parsec String () Coord
coord =
    curry Coord <$> (integer <* string ", ") <*> integer <* optional (char '\n')

parseInput :: String -> [Coord]
parseInput input =
    case result of
        Left e       -> error $ show e
        Right coords -> coords
  where
    result = parse (many coord) "" input

-- Actual problem
data Area
    = Finite Integer
    | Infinite
    deriving (Show)

isFinite :: Area -> Bool
isFinite Infinite = False
isFinite _        = True

newtype Areas = Areas
    { runAreas :: M.Map Integer Area
    } deriving (Show)

area (Finite a) = a
area _          = error "Cannot get infinite areas"

largestUnsafeArea :: [Coord] -> Integer
largestUnsafeArea coords =
    maximum . map area . M.elems . M.filter isFinite . runAreas $ areas
  where
    width = maximum $ getX <$> coords
    height = maximum $ getY <$> coords
    areas = execState (getAreas width height coords) (Areas M.empty)

getAreas :: Integer -> Integer -> [Coord] -> State Areas ()
getAreas width height coords = forM_ (board width height) step
  where
    step coord =
        case closest coords coord of
            Just i ->
                if isBorder coord
                    then modify $ setInfinite i
                    else modify $ increment i
            Nothing -> return ()
    isBorder (Coord (x, y)) = x == 0 || x == width || y == 0 || y == height
    setInfinite i = Areas . M.insert i Infinite . runAreas
    increment i = Areas . M.alter upsert i . runAreas
    upsert ma =
        case ma of
            Just a ->
                case a of
                    Finite v -> Just $ Finite (v + 1)
                    Infinite -> Just Infinite
            Nothing -> Just $ Finite 1

board :: Integer -> Integer -> [Coord]
board width height = [Coord (x, y) | x <- [0 .. width], y <- [0 .. height]]

closest :: [Coord] -> Coord -> Maybe Integer
closest coords coord =
    indexIfUnique .
    sortBy (compare `on` snd) . zip [0 ..] . map (distance coord) $
    coords
  where
    indexIfUnique ((ia, da):(ib, db):rs) =
        if da == db
            then Nothing
            else Just ia
    indexIfUnique ((ia, da):rs) = Just ia
    indexIfUnique [] = Nothing

safeArea :: [Coord] -> Integer
safeArea coords = foldl countSafe 0 (board width height)
  where
    width = maximum $ getX <$> coords
    height = maximum $ getY <$> coords
    countSafe safe coord =
        if totalDistance coords coord < 10000
            then safe + 1
            else safe

totalDistance :: [Coord] -> Coord -> Integer
totalDistance coords coord = sum . map (distance coord) $ coords

day6 :: IO ()
day6 = do
    input <- parseInput <$> loadInput
    print $ largestUnsafeArea input
    print $ safeArea input
