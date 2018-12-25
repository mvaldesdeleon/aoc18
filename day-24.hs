{-# LANGUAGE TemplateHaskell #-}

import           Control.Lens
import           Control.Monad   (join)
import           Data.Function   (on)
import qualified Data.List.Safe  as L
import qualified Data.Map.Strict as M
import           Data.Maybe      (catMaybes, fromMaybe)
import           Data.Monoid     ((<>))
import qualified Data.Set        as S
import           Text.Parsec     (Parsec, anyChar, between, char, digit, endBy1,
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

makePrisms ''AttackModifier

data Group = Group
    { _units           :: Integer
    , _hitPoints       :: Integer
    , _attackModifiers :: [AttackModifier]
    , _attackDamage    :: Integer
    , _attackType      :: AttackType
    , _initiative      :: Integer
    } deriving (Show, Eq)

makeLenses ''Group

isImmuneTo :: Group -> AttackType -> Bool
isImmuneTo g at = anyOf (attackModifiers . traverse . _ImmuneTo) (== at) g

isWeakTo :: Group -> AttackType -> Bool
isWeakTo g at = anyOf (attackModifiers . traverse . _WeakTo) (== at) g

effectivePower :: Group -> Integer
effectivePower g = max 0 (g ^. units * g ^. attackDamage)

damages :: Group -> Group -> Integer
damages atk def
    | def `isImmuneTo` (atk ^. attackType) = 0
    | def `isWeakTo` (atk ^. attackType) = effectivePower atk * 2
    | otherwise = effectivePower atk

attacks :: Group -> Group -> Group
attacks atk def = def & units %~ (max 0 . subtract unitsLost)
  where
    unitsLost = (atk `damages` def) `div` (def ^. hitPoints)

data Player = Player
    { _name   :: String
    , _groups :: [Group]
    } deriving (Show)

makeLenses ''Player

data PlayerGroup =
    PlayerGroup String
                Integer
                Group
    deriving (Show)

instance Eq PlayerGroup where
    (PlayerGroup pa ia _) == (PlayerGroup pb ib _) = pa == pb && ia == ib

same :: PlayerGroup -> PlayerGroup -> Bool
same pga pgb = pga == pgb && ((==) `on` grp) pga pgb

instance Ord PlayerGroup where
    (PlayerGroup pa ia _) `compare` (PlayerGroup pb ib _) =
        pa `compare` pb <> ia `compare` ib

grp :: PlayerGroup -> Group
grp (PlayerGroup _ _ g) = g

playerName :: PlayerGroup -> String
playerName (PlayerGroup n _ _) = n

toPlayerGroups :: Player -> [PlayerGroup]
toPlayerGroups p =
    uncurry (PlayerGroup (p ^. name)) <$> zip [1 ..] (p ^. groups)

toPlayers :: [PlayerGroup] -> [Player]
toPlayers = map toPlayer . L.groupBy ((==) `on` playerName) . L.sort
  where
    toPlayer :: [PlayerGroup] -> Player
    toPlayer pgs =
        Player
            (maybe (error "No PlayerGroup in list") playerName (L.head pgs))
            (grp <$> pgs)

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

parseInput :: String -> [PlayerGroup]
parseInput input =
    case parse (parsePlayer `sepBy1` newline) "" input of
        Left e   -> error $ show e
        Right ps -> ps >>= toPlayerGroups

selectTargets :: [PlayerGroup] -> [Maybe PlayerGroup]
selectTargets pgs = map (`M.lookup` targetMap) pgs
  where
    (targetMap, _) =
        foldl
            pickTarget
            (M.empty, S.fromList pgs)
            (L.sortBy (groupEffectivePower <> groupInitiative) pgs)
    groupEffectivePower = flip compare `on` effectivePower . grp
    groupInitiative = flip compare `on` _initiative . grp
    pickTarget (assigned, available) pg =
        case L.head targets of
            Just tg -> (M.insert pg tg assigned, S.delete tg available)
            Nothing -> (assigned, available)
      where
        targets =
            L.sortBy
                (damageTakenBy pg <> groupEffectivePower <> groupInitiative) .
            filter (\tg -> pg `enemiesOf` tg && pg `groupDamages` tg > 0) .
            S.toList $
            available
        enemiesOf = (/=) `on` playerName
        groupDamages = damages `on` grp
        damageTakenBy pg = flip compare `on` (pg `groupDamages`)

attack :: [PlayerGroup] -> [Maybe PlayerGroup] -> [PlayerGroup]
attack pg tg = foldl individualAttack pg . L.sortBy groupInitiative $ zip pg tg
  where
    groupInitiative = flip compare `on` _initiative . grp . fst
    individualAttack pgs (pg, mtg) =
        case mtg of
            Just tg -> performAttack pg tg pgs
            Nothing -> pgs
    performAttack atk def pgs =
        let mna = L.find (== atk) pgs
            mnd = L.find (== def) pgs
        in case (mna, mnd) of
               (Just na, Just nd) -> replaceGroup nd (na `groupAttacks` nd) pgs
               _ -> pgs
    groupAttacks = attacks `on` grp
    replaceGroup pg@(PlayerGroup p i _) g =
        (\(init, _:tail) -> init ++ PlayerGroup p i g : tail) . L.break (== pg)

removeDefeated :: [PlayerGroup] -> [PlayerGroup]
removeDefeated = filter (\pg -> grp pg ^. units > 0)

fight :: [PlayerGroup] -> [PlayerGroup]
fight pgs = removeDefeated $ attack pgs (selectTargets pgs)

sumUnits :: [PlayerGroup] -> Integer
sumUnits pgs = sum (map groupUnits pgs)
  where
    groupUnits pg = grp pg ^. units

boost :: String -> Integer -> [PlayerGroup] -> [PlayerGroup]
boost player amount [] = []
boost player amount (pg@(PlayerGroup p i g):pgs)
    | p == player =
        PlayerGroup p i (g & attackDamage +~ amount) : boost player amount pgs
    | otherwise = pg : boost player amount pgs

result :: [[PlayerGroup]] -> [PlayerGroup]
result =
    fromMaybe (error "No result") .
    L.head . dropWhile ((> 1) . length . toPlayers)

result' :: [[PlayerGroup]] -> Maybe [PlayerGroup]
result' (pga:pgb:pgs) =
    if all (uncurry same) (zip pga pgb) && length pga == length pgb
        then if length (toPlayers pga) == 1
                 then Just pga
                 else Nothing
        else result' (pgb : pgs)
result' _ = Nothing

main :: IO ()
main = do
    input <- parseInput <$> loadInput
    print $ sumUnits . result . iterate fight $ input
    let results =
            result' . iterate fight <$>
            (boost "Immune System" <$> [1 ..] <*> [input])
    print $
        sumUnits .
        fromMaybe (error "No results") .
        L.head .
        dropWhile
            ((/= "Immune System") .
             playerName . fromMaybe (error "Empty result") . L.head) $
        catMaybes results
