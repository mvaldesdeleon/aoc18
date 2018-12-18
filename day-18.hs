{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE TemplateHaskell #-}

import           Control.Lens
import           Data.List       (genericLength)
import qualified Data.Map.Strict as M

data Acre
    = Open
    | Trees
    | Lumberyard
    deriving (Show, Eq)

charToAcre :: Char -> Acre
charToAcre =
    \case
        '.' -> Open
        '|' -> Trees
        '#' -> Lumberyard

data Size = Size
    { _width  :: Integer
    , _height :: Integer
    } deriving (Show, Eq)

data Position = Position
    { _x :: Integer
    , _y :: Integer
    } deriving (Show, Eq, Ord)

makeLenses ''Size

makeLenses ''Position

type Area = M.Map Position Acre

loadInput :: IO String
loadInput = readFile "inputs/day-18.txt"

mapSnd :: (a -> b) -> (c, a) -> (c, b)
mapSnd f (c, a) = (c, f a)

parseArea :: Size -> String -> Area
parseArea size = M.fromList . map (mapSnd charToAcre) . addPosition size

addPosition :: Size -> String -> [(Position, Char)]
addPosition (Size width height) =
    zip [Position x y | y <- [0 .. height - 1], x <- [0 .. width - 1]]

parseInput :: String -> Area
parseInput input = parseArea size clean
  where
    size = Size width height
    width = genericLength . (!! 0) . lines $ input
    height = genericLength . lines $ input
    clean = filter (/= '\n') input

main :: IO ()
main = do
    input <- parseInput <$> loadInput
    print $ input
