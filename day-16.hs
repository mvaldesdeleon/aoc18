{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

import           Control.Lens
import           Data.Bits    ((.&.), (.|.))
import           Data.List    (genericLength)
import           Data.Maybe   (fromMaybe)
import           Text.Parsec

newtype CPU = CPU
    { _registries :: [Integer]
    } deriving (Show, Eq)

data Instruction = Instruction
    { _opcode :: Integer
    , _a      :: Integer
    , _b      :: Integer
    , _c      :: Integer
    } deriving (Show, Eq)

data Entry = Entry
    { _input       :: CPU
    , _instruction :: Instruction
    , _output      :: CPU
    } deriving (Show)

data Problem = Problem
    { _entries      :: [Entry]
    , _instructions :: [Instruction]
    } deriving (Show)

makeLenses ''CPU

makeLenses ''Instruction

makeLenses ''Entry

loadInput :: IO String
loadInput = readFile "inputs/day-16.txt"

fI :: (Integral a, Num b) => a -> b
fI = fromIntegral

opCount :: Entry -> Integer
opCount Entry {..} =
    genericLength $ filter (== _output) (allOps _input _instruction)

number :: Parsec String () Integer
number = read <$> (many space *> many1 digit)

parseRegistries :: Parsec String () [Integer]
parseRegistries = between (string "[") (string "]") (number `sepBy1` string ",")

parseCPU :: String -> Parsec String () CPU
parseCPU label =
    string label *> string ":" *> many space *> (CPU <$> parseRegistries)

parseInstruction :: Parsec String () Instruction
parseInstruction = Instruction <$> number <*> number <*> number <*> number

parseEntry :: Parsec String () Entry
parseEntry =
    Entry <$> (parseCPU "Before" <* newline) <*> (parseInstruction <* newline) <*>
    parseCPU "After"

parseProblem :: Parsec String () Problem
parseProblem =
    Problem <$> (many1 (parseEntry <* count 2 newline)) <*>
    (many newline *> parseInstruction `sepBy1` newline)

parseInput :: String -> Problem
parseInput input =
    case result of
        Left e  -> error $ show e
        Right r -> r
  where
    result = parse parseProblem "" input

allOps :: CPU -> Instruction -> [CPU]
allOps cpu (Instruction _ a b c) =
    [ addr a b c
    , addi a b c
    , mulr a b c
    , muli a b c
    , banr a b c
    , bani a b c
    , borr a b c
    , bori a b c
    , setr a b c
    , seti a b c
    , gtir a b c
    , gtri a b c
    , gtrr a b c
    , eqir a b c
    , eqri a b c
    , eqrr a b c
    ] <*>
    [cpu]

addr :: Integer -> Integer -> Integer -> CPU -> CPU
addr a b c cpu =
    let ra = fromMaybe 0 $ cpu ^? registries . ix (fI a)
        rb = fromMaybe 0 $ cpu ^? registries . ix (fI b)
    in cpu & (registries . ix (fI c)) .~ (ra + rb)

addi :: Integer -> Integer -> Integer -> CPU -> CPU
addi a b c cpu =
    let ra = fromMaybe 0 $ cpu ^? registries . ix (fI a)
    in cpu & (registries . ix (fI c)) .~ (ra + b)

mulr :: Integer -> Integer -> Integer -> CPU -> CPU
mulr a b c cpu =
    let ra = fromMaybe 0 $ cpu ^? registries . ix (fI a)
        rb = fromMaybe 0 $ cpu ^? registries . ix (fI b)
    in cpu & (registries . ix (fI c)) .~ (ra * rb)

muli :: Integer -> Integer -> Integer -> CPU -> CPU
muli a b c cpu =
    let ra = fromMaybe 0 $ cpu ^? registries . ix (fI a)
    in cpu & (registries . ix (fI c)) .~ (ra * b)

banr :: Integer -> Integer -> Integer -> CPU -> CPU
banr a b c cpu =
    let ra = fromMaybe 0 $ cpu ^? registries . ix (fI a)
        rb = fromMaybe 0 $ cpu ^? registries . ix (fI b)
    in cpu & (registries . ix (fI c)) .~ (ra .&. rb)

bani :: Integer -> Integer -> Integer -> CPU -> CPU
bani a b c cpu =
    let ra = fromMaybe 0 $ cpu ^? registries . ix (fI a)
    in cpu & (registries . ix (fI c)) .~ (ra .&. b)

borr :: Integer -> Integer -> Integer -> CPU -> CPU
borr a b c cpu =
    let ra = fromMaybe 0 $ cpu ^? registries . ix (fI a)
        rb = fromMaybe 0 $ cpu ^? registries . ix (fI b)
    in cpu & (registries . ix (fI c)) .~ (ra .|. rb)

bori :: Integer -> Integer -> Integer -> CPU -> CPU
bori a b c cpu =
    let ra = fromMaybe 0 $ cpu ^? registries . ix (fI a)
    in cpu & (registries . ix (fI c)) .~ (ra .|. b)

setr :: Integer -> Integer -> Integer -> CPU -> CPU
setr a _ c cpu =
    let ra = fromMaybe 0 $ cpu ^? registries . ix (fI a)
    in cpu & (registries . ix (fI c)) .~ ra

seti :: Integer -> Integer -> Integer -> CPU -> CPU
seti a _ c cpu = cpu & (registries . ix (fI c)) .~ a

gtir :: Integer -> Integer -> Integer -> CPU -> CPU
gtir a b c cpu =
    let rb = fromMaybe 0 $ cpu ^? registries . ix (fI b)
    in cpu & (registries . ix (fI c)) .~
       (if a > rb
            then 1
            else 0)

gtri :: Integer -> Integer -> Integer -> CPU -> CPU
gtri a b c cpu =
    let ra = fromMaybe 0 $ cpu ^? registries . ix (fI a)
    in cpu & (registries . ix (fI c)) .~
       (if ra > b
            then 1
            else 0)

gtrr :: Integer -> Integer -> Integer -> CPU -> CPU
gtrr a b c cpu =
    let ra = fromMaybe 0 $ cpu ^? registries . ix (fI a)
        rb = fromMaybe 0 $ cpu ^? registries . ix (fI b)
    in cpu & (registries . ix (fI c)) .~
       (if ra > rb
            then 1
            else 0)

eqir :: Integer -> Integer -> Integer -> CPU -> CPU
eqir a b c cpu =
    let rb = fromMaybe 0 $ cpu ^? registries . ix (fI b)
    in cpu & (registries . ix (fI c)) .~
       (if a == rb
            then 1
            else 0)

eqri :: Integer -> Integer -> Integer -> CPU -> CPU
eqri a b c cpu =
    let ra = fromMaybe 0 $ cpu ^? registries . ix (fI a)
    in cpu & (registries . ix (fI c)) .~
       (if ra == b
            then 1
            else 0)

eqrr :: Integer -> Integer -> Integer -> CPU -> CPU
eqrr a b c cpu =
    let ra = fromMaybe 0 $ cpu ^? registries . ix (fI a)
        rb = fromMaybe 0 $ cpu ^? registries . ix (fI b)
    in cpu & (registries . ix (fI c)) .~
       (if ra == rb
            then 1
            else 0)

main :: IO ()
main = do
    input <- parseInput <$> loadInput
    print $ length . filter (>= 3) $ opCount <$> _entries input
