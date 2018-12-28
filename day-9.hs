{-# LANGUAGE RecordWildCards #-}

import           Control.Monad.Loops
import           Control.Monad.State
import qualified Data.List.Zipper    as Z
import qualified Data.Map.Strict     as M
import           Text.Parsec         (Parsec, digit, many1, newline, optional,
                                      parse, string)

loadInput :: IO String
loadInput = readFile "inputs/day-9.txt"

data GameConfig = GameConfig
    { cfgPlayers :: Integer
    , cfgMarbles :: Integer
    } deriving (Show)

number :: Parsec String () Integer
number = read <$> many1 digit

gameConfig :: Parsec String () GameConfig
gameConfig =
    GameConfig <$> (number <* string " players; last marble is worth ") <*>
    (number <* string " points") <*
    optional newline

parseInput :: String -> GameConfig
parseInput input =
    case result of
        Left e     -> error $ show e
        Right game -> game
  where
    result = parse gameConfig "" input

data Game = Game
    { gConfig     :: GameConfig
    , gNextMarble :: Integer
    , gPlayers    :: Z.Zipper Integer
    , gCircle     :: Z.Zipper Integer
    } deriving (Show)

startGame :: GameConfig -> Game
startGame gc@GameConfig {..} =
    Game
        gc
        1
        (Z.fromList . replicate (fromIntegral cfgPlayers) $ 0)
        (Z.fromList [0])

left :: Eq a => Z.Zipper a -> Z.Zipper a
left z =
    if lz == z
        then Z.left . Z.end $ z
        else lz
  where
    lz = Z.left z

right :: Z.Zipper a -> Z.Zipper a
right z =
    if Z.endp rz
        then Z.start z
        else rz
  where
    rz = Z.right z

playTurn :: State Game ()
playTurn = do
    nextMarble <- gets gNextMarble
    if (nextMarble `mod` 23) == 0
        then do
            circle <- gets (fpow left 7 . gCircle)
            modify
                (\g@Game {..} ->
                     g
                         { gPlayers =
                               Z.replace
                                   (Z.cursor gPlayers + Z.cursor circle +
                                    nextMarble)
                                   gPlayers
                         , gCircle = Z.delete circle
                         })
        else modify
                 (\g@Game {..} ->
                      g {gCircle = Z.insert nextMarble (fpow right 2 gCircle)})
    modify
        (\g@Game {..} ->
             g {gPlayers = right gPlayers, gNextMarble = gNextMarble + 1})

fpow :: (a -> a) -> Integer -> a -> a
fpow f n a = iterate f a !! fromInteger n

gameOver :: State Game Bool
gameOver = do
    lastMarble <- gets (cfgMarbles . gConfig)
    nextMarble <- gets gNextMarble
    return $ nextMarble > lastMarble

playGame :: State Game ()
playGame = playTurn `untilM_` gameOver

highScore :: GameConfig -> Integer
highScore gc = maximum . Z.toList . gPlayers $ endGame
  where
    endGame = execState playGame (startGame gc)

alter :: GameConfig -> GameConfig
alter gc@GameConfig {..} = gc {cfgMarbles = cfgMarbles * 100}

main :: IO ()
main = do
    input <- parseInput <$> loadInput
    print $ highScore input
    print $ highScore (alter input)
