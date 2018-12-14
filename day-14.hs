{-# LANGUAGE RecordWildCards #-}

import           Control.Monad       (replicateM_)
import           Control.Monad.Loops (iterateUntil)
import           Control.Monad.State (State, execState, gets, modify)
import           Data.Char           (intToDigit)
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

digits :: Integral x => x -> [x]
digits 0 = []
digits x = digits (x `div` 10) ++ [x `mod` 10]

brewUntil :: Integer -> State Recipes Integer
brewUntil count = iterateUntil (>= count) make

tenScores :: Integer -> String
tenScores input = sumSeries 10 input scores
  where
    scores = rScores $ execState (brewUntil $ input + 10) initial

sumSeries :: Integer -> Integer -> Scores -> String
sumSeries count i scores =
    map (intToDigit . fromIntegral . (scores M.!)) [i .. i + count - 1]

main :: IO ()
main = do
    input <- parseInput <$> loadInput
    print $ tenScores input
