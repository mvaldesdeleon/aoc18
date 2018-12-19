{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

import           Control.Lens
import           Data.Bits       ((.&.), (.|.))
import           Data.List       (genericLength)
import qualified Data.Map.Strict as M
import           Data.Maybe      (fromMaybe)
import           Text.Parsec     (Parsec, digit, many, many1, newline, parse,
                                  sepBy1, space, string, try, (<?>), (<|>))

data CPU = CPU
    { _registries :: [Integer]
    , _ipIx       :: Integer
    , _program    :: [Instruction]
    } deriving (Show, Eq)

data OpCode
    = ADDR
    | ADDI
    | MULR
    | MULI
    | BANR
    | BANI
    | BORR
    | BORI
    | SETR
    | SETI
    | GTIR
    | GTRI
    | GTRR
    | EQRI
    | EQIR
    | EQRR
    deriving (Show, Eq, Ord)

data Instruction = Instruction
    { _opcode :: OpCode
    , _a      :: Integer
    , _b      :: Integer
    , _c      :: Integer
    } deriving (Show, Eq)

makeLenses ''CPU

makeLenses ''Instruction

loadInput :: IO String
loadInput = readFile "inputs/day-19.txt"

fI :: (Integral a, Num b) => a -> b
fI = fromIntegral

number :: Parsec String () Integer
number = read <$> (many space *> many1 digit)

parseDecaration :: Parsec String () Integer
parseDecaration = string "#ip " *> number

parseOpCode :: Parsec String () OpCode
parseOpCode =
    pADDR <|> pADDI <|> pMULR <|> pMULI <|> pBANR <|> pBANI <|> pBORR <|> pBORI <|>
    pSETR <|>
    pSETI <|>
    pGTIR <|>
    pGTRI <|>
    pGTRR <|>
    pEQRI <|>
    pEQIR <|>
    pEQRR <?>
    "opcode"
  where
    pADDR = try (ADDR <$ string "addr")
    pADDI = try (ADDI <$ string "addi")
    pMULR = try (MULR <$ string "mulr")
    pMULI = try (MULI <$ string "muli")
    pBANR = try (BANR <$ string "banr")
    pBANI = try (BANI <$ string "bani")
    pBORR = try (BORR <$ string "borr")
    pBORI = try (BORI <$ string "bori")
    pSETR = try (SETR <$ string "setr")
    pSETI = try (SETI <$ string "seti")
    pGTIR = try (GTIR <$ string "gtir")
    pGTRI = try (GTRI <$ string "gtri")
    pGTRR = try (GTRR <$ string "gtrr")
    pEQRI = try (EQRI <$ string "eqri")
    pEQIR = try (EQIR <$ string "eqir")
    pEQRR = try (EQRR <$ string "eqrr")

parseInstruction :: Parsec String () Instruction
parseInstruction =
    Instruction <$> parseOpCode <*> (many1 space *> number) <*>
    (many1 space *> number) <*>
    (many1 space *> number)

parseCPU :: Parsec String () CPU
parseCPU =
    CPU <$> pure [0, 0, 0, 0, 0, 0] <*> (parseDecaration <* newline) <*>
    parseInstruction `sepBy1`
    newline

parseInput :: String -> CPU
parseInput input =
    case result of
        Left e  -> error $ show e
        Right r -> r
  where
    result = parse parseCPU "" input

operations :: M.Map OpCode (Integer -> Integer -> Integer -> CPU -> CPU)
operations =
    M.fromList $
    zip [ ADDR
        , ADDI
        , MULR
        , MULI
        , BANR
        , BANI
        , BORR
        , BORI
        , SETR
        , SETI
        , GTIR
        , GTRI
        , GTRR
        , EQRI
        , EQIR
        , EQRR
        ]
        [ addr
        , addi
        , mulr
        , muli
        , banr
        , bani
        , borr
        , bori
        , setr
        , seti
        , gtir
        , gtri
        , gtrr
        , eqir
        , eqri
        , eqrr
        ]

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

halted :: CPU -> Bool
halted cpu =
    let ip = fromMaybe 0 $ cpu ^? registries . ix (fI $ cpu ^. ipIx)
    in ip >= genericLength (cpu ^. program)

execute :: CPU -> CPU
execute cpu =
    let ip = fromMaybe 0 $ cpu ^? registries . ix (fI $ cpu ^. ipIx)
        Just ins = cpu ^? (program . ix (fI ip))
    in ((registries . ix (fI $ cpu ^. ipIx)) %~ succ) . executeInstruction ins $
       cpu

executeInstruction :: Instruction -> CPU -> CPU
executeInstruction Instruction {..} = (operations M.! _opcode) _a _b _c

run :: CPU -> CPU
run cpu =
    if halted cpu
        then cpu
        else run $ execute cpu

main :: IO ()
main = do
    input <- parseInput <$> loadInput
    print $ run input ^. registries
    print $ run (input & (registries . ix 0) .~ 1) ^. registries
