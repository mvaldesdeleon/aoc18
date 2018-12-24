{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

import           Control.Lens
import           Control.Monad (join)
import           Text.Parsec   (Parsec, anyChar, between, char, digit, endBy1,
                                many1, manyTill, newline, optional, parse,
                                sepBy1, space, string, try, (<?>), (<|>))

loadInput = readFile "inputs/day-24.txt"

data AttackType
    = Cold
    | Fire
    | Radiation
    | Slashing
    | Bludgeoning
    deriving (Show, Eq)

data AttackModifier
    = WeakTo AttackType
    | ImmuneTo AttackType
    deriving (Show, Eq)

data Group = Group
    { _units           :: Integer
    , _hitPoints       :: Integer
    , _attackModifiers :: [AttackModifier]
    , _attackDamage    :: Integer
    , _attackType      :: AttackType
    , _initiative      :: Integer
    } deriving (Show)

makeLenses ''Group

data Player = Player
    { _name   :: String
    , _groups :: [Group]
    } deriving (Show)

parseAttackType :: Parsec String () AttackType
parseAttackType =
    cold <|> fire <|> radiation <|> slashing <|> bludgeoning <?> "attack type"
  where
    cold = Cold <$ try (string "cold")
    fire = Fire <$ try (string "fire")
    radiation = Radiation <$ try (string "radiation")
    slashing = Slashing <$ try (string "slashing")
    bludgeoning = Bludgeoning <$ try (string "bludgeoning")

parseAttackModifiers :: Parsec String () [AttackModifier]
parseAttackModifiers =
    between
        (char '(')
        (char ')')
        (join <$> parseModifierGroup `sepBy1` string "; ")
  where
    parseModifierGroup =
        parseWeakToGroup <|> parseImmuneToGroup <?> "modifier group"
    parseWeakToGroup =
        string "weak to " *> (WeakTo <$> parseAttackType) `sepBy1` string ", "
    parseImmuneToGroup =
        string "immune to " *>
        (ImmuneTo <$> parseAttackType) `sepBy1` string ", "

parseGroup :: Parsec String () Group
parseGroup =
    Group <$> number <* string " units each with " <*> number <*
    string " hit points " <*>
    (try parseAttackModifiers <|> pure []) <*
    optional space <*
    string "with an attack that does " <*>
    number <*
    space <*>
    parseAttackType <*
    string " damage at initiative " <*>
    number
  where
    number = read <$> many1 digit

parsePlayer :: Parsec String () Player
parsePlayer = Player <$> parseName <*> parseGroup `endBy1` newline
  where
    parseName = anyChar `manyTill` char ':' <* newline

parseInput :: String -> [Player]
parseInput input =
    case parse (parsePlayer `sepBy1` newline) "" input of
        Left e   -> error $ show e
        Right ps -> ps

makeLenses ''Player

main :: IO ()
main = do
    input <- parseInput <$> loadInput
    print input
