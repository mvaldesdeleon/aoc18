{-# LANGUAGE RecordWildCards #-}

import           Control.Monad       (replicateM_)
import           Control.Monad.Loops (iterateUntil, untilM_)
import           Control.Monad.State (State, execState, gets, modify)
import           Data.Char           (digitToInt, intToDigit)
import           Data.List           (genericLength)
import qualified Data.Map.Strict     as M

loadInput :: IO String
loadInput = readFile "inputs/day-14.txt"

parseInput :: String -> Integer
parseInput = read

type Scores = M.Map Integer Integer

data Recipes = Recipes
    { rScores :: Scores
    , rFirst  :: Integer
    , rSecond :: Integer
    } deriving (Show)

initial :: Recipes
initial = Recipes (M.fromList [(0, 3), (1, 7)]) 0 1

make :: State Recipes Integer
make = do
    modify
        (\r@Recipes {..} ->
             r {rScores = insert (newScores rFirst rSecond rScores) rScores})
    modify
        (\r@Recipes {..} ->
             r
             { rFirst = advance rFirst rScores
             , rSecond = advance rSecond rScores
             })
    gets (fromIntegral . M.size . rScores)

newScores :: Integer -> Integer -> Scores -> [Integer]
newScores first second scores =
    digits $ (scores M.! first) + (scores M.! second)

insert :: [Integer] -> Scores -> Scores
insert newScores scores =
    foldl f scores (zip [fromIntegral $ M.size scores ..] newScores)
  where
    f = flip $ uncurry M.insert

advance :: Integer -> Scores -> Integer
advance i scores = (1 + i + scores M.! i) `mod` fromIntegral (M.size scores)

digits :: Integer -> [Integer]
digits = map (fromIntegral . digitToInt) . show

brewMany :: Integer -> State Recipes Integer
brewMany count = iterateUntil (>= count) make

tenScores :: Integer -> String
tenScores input = sumSeries 10 input scores
  where
    scores = rScores $ execState (brewMany $ input + 10) initial

sumSeries :: Integer -> Integer -> Scores -> String
sumSeries count i scores =
    map (intToDigit . fromIntegral . (scores M.!)) [i .. i + count - 1]

brewSequence :: Integer -> State Recipes ()
brewSequence seq = make `untilM_` contains seq

contains :: Integer -> State Recipes Bool
contains seq = do
    scores <- gets rScores
    let s = show seq
    let l = genericLength s
    let i = fromIntegral (M.size scores) - l
    return $
        (i >= 0 && sumSeries l i scores == s) ||
        (i >= 1 && sumSeries l (i - 1) scores == s)

toTheLeft :: Integer -> Integer
toTheLeft input = find input scores
  where
    scores = rScores $ execState (brewSequence input) initial

find :: Integer -> Scores -> Integer
find seq scores =
    if sumSeries l i scores == s
        then i
        else i - 1
  where
    s = show seq
    l = genericLength s
    i = fromIntegral (M.size scores) - l

main :: IO ()
main = do
    input <- parseInput <$> loadInput
    print $ tenScores input
    print $ toTheLeft input
