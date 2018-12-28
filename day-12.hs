{-# LANGUAGE RecordWildCards #-}

import           Data.List
import           Text.Parsec (Parsec, char, count, many, newline, optional,
                              parse, sepBy, string, (<|>))

data Pot
    = Empty
    | Plant
    deriving (Eq)

instance Show Pot where
    show Empty = "."
    show Plant = "#"
    showList ps s = intercalate [] (map show ps) ++ s

isEmpty :: Pot -> Bool
isEmpty Empty = True
isEmpty _     = False

data Rule
    = Birth [Pot]
    | Death [Pot]

isBirth :: Rule -> Bool
isBirth (Birth r_) = True
isBirth _          = False

instance Show Rule where
    show (Birth ps) = show ps ++ " => #"
    show (Death ps) = show ps ++ " => ."

pots :: Rule -> [Pot]
pots (Birth ps) = ps
pots (Death ps) = ps

pot :: Parsec String () Pot
pot = (Empty <$ char '.') <|> (Plant <$ char '#')

initialState :: Parsec String () [Pot]
initialState = string "initial state: " *> many pot <* optional newline

rule :: Parsec String () Rule
rule = mkRule <$> count 5 pot <*> (string " => " *> pot)
  where
    mkRule pots res =
        case res of
            Empty -> Death pots
            Plant -> Birth pots

data Config = Config
    { cfgState :: [Pot]
    , cfgIndex :: Integer
    , cfgRules :: [Rule]
    } deriving (Show)

config :: Parsec String () Config
config = mkConfig <$> initialState <*> (newline *> (f <$> rule `sepBy` newline))
  where
    mkConfig = flip Config 0
    f = filter isBirth

loadInput :: IO String
loadInput = readFile "inputs/day-12.txt"

parseInput :: String -> Config
parseInput input =
    case res of
        Left e    -> error $ show e
        Right cfg -> cfg
  where
    res = parse config "" input

extend cfg@Config {..} =
    cfg
        { cfgIndex = cfgIndex - 5
        , cfgState = replicate 5 Empty ++ cfgState ++ replicate 5 Empty
        }

grow :: Config -> Config
grow cfg@Config {..} =
    cfg {cfgIndex = cfgIndex + 2, cfgState = f cfgRules cfgState}
  where
    f rules (p1:p2:p3:p4:p5:ps) =
        m rules [p1, p2, p3, p4, p5] : f rules (p2 : p3 : p4 : p5 : ps)
    f rules _ = []
    m :: [Rule] -> [Pot] -> Pot
    m rules ps =
        case find ((== ps) . pots) rules of
            Just r ->
                case r of
                    Birth _ -> Plant
                    Death _ -> Empty
            Nothing -> Empty

trimEmpty :: Config -> Config
trimEmpty cfg@Config {..} =
    cfg {cfgIndex = cfgIndex + genericLength left, cfgState = reverse final}
  where
    (left, rest) = span isEmpty cfgState
    (right, final) = span isEmpty (reverse rest)

generation :: Config -> Config
generation = trimEmpty . grow . extend

value :: Config -> Integer
value Config {..} = foldl f 0 (zip [cfgIndex ..] cfgState)
  where
    f v (i, p) =
        case p of
            Plant -> v + i
            Empty -> v

fpow :: (a -> a) -> Integer -> a -> a
fpow f n a = iterate f a !! fromInteger n

main :: IO ()
main = do
    input <- parseInput <$> loadInput
    print $ value . fpow generation 20 $ input
    let (x0, y0) = (100, value . fpow generation 100 $ input)
    let (x1, y1) = (110, value . fpow generation 110 $ input)
    let (dx, dy) = (x1 - x0, y1 - y0)
    let m = dy `div` dx
    let b = y0 - x0 * m
    print $ m * 50000000000 + b
