{-# LANGUAGE LambdaCase #-}

import           Control.Monad   (join)
import           Data.List       (nub)
import qualified Data.Map.Strict as M
import qualified Data.Set        as S
import           Text.Parsec     (Parsec, between, char, many, parse, sepBy,
                                  try, (<?>), (<|>))

loadInput :: IO String
loadInput = readFile "inputs/day-20.txt"

parseStep :: Parsec String () Term
parseStep =
    Step <$>
    (try (N <$ char 'N') <|> try (S <$ char 'S') <|> try (W <$ char 'W') <|>
     try (E <$ char 'E') <?> "step")

parseDetourOrBranch :: Parsec String () Term
parseDetourOrBranch = mkDetourOrBranch <$> parseContents
  where
    parseContents = between (char '(') (char ')') (parseExpr `sepBy` char '|')
    mkDetourOrBranch [] = Detour []
    mkDetourOrBranch exprs =
        if null . last $ exprs
            then Detour $ init exprs
            else Branch exprs

parseTerm :: Parsec String () Term
parseTerm = try parseStep <|> try parseDetourOrBranch <?> "term"

parseExpr :: Parsec String () Expr
parseExpr = many parseTerm

parseDirections :: Parsec String () Expr
parseDirections = between (char '^') (char '$') parseExpr

parseInput :: String -> Expr
parseInput input =
    case res of
        Left e     -> error $ show e
        Right expr -> expr
  where
    res = parse parseDirections "" input

data Direction
    = N
    | S
    | W
    | E
    deriving (Show, Eq)

type Expr = [Term]

data Term
    = Step Direction
    | Branch [Expr]
    | Detour [Expr]
    deriving (Show, Eq)

-- Sanity check
hasEmptyExprs :: Expr -> Bool
hasEmptyExprs = any termHasEmptyExprs
  where
    termHasEmptyExprs =
        \case
            Branch exprs -> null exprs || any hasEmptyExprs exprs
            Detour exprs -> null exprs || any hasEmptyExprs exprs
            _ -> False

-- Exploratory
hasTermsAfterBranch :: Expr -> Bool
hasTermsAfterBranch =
    (||) <$> exprHasTermsAfterBranch <*> any termHasTermsAfterBranch
  where
    termHasTermsAfterBranch =
        \case
            Branch exprs -> any hasTermsAfterBranch exprs
            Detour exprs -> any hasTermsAfterBranch exprs
            _ -> False
    exprHasTermsAfterBranch (t1:t2:ts) =
        case t1 of
            Branch _ -> True
            _        -> exprHasTermsAfterBranch (t2 : ts)
    exprHasTermsAfterBranch _ = False

-- Validation
hasDetours :: Expr -> Bool
hasDetours = any termHasDetours
  where
    termHasDetours =
        \case
            Branch exprs -> any hasDetours exprs
            Detour exprs -> True
            _ -> False

-- Validation
hasBranches :: Expr -> Bool
hasBranches = any termHasBranches
  where
    termHasBranches =
        \case
            Branch exprs -> True
            Detour exprs -> any hasBranches exprs
            _ -> False

-- We can eliminate all Detours by turning them into Branches
-- A(B|C|)D = A(B|C|D)
detourToBranch :: Expr -> Expr
detourToBranch [] = []
detourToBranch (t:ts) =
    case t of
        Branch exprs -> Branch (map detourToBranch exprs) : detourToBranch ts
        Detour exprs -> [Branch (map detourToBranch $ exprs ++ [ts])]
        _            -> t : detourToBranch ts

-- Not really necesary but for the sake of completion...
-- We can move all branches to the end of an Expression
-- A(B|C)D = A(BD|CD)
branchToBranch :: Expr -> Expr
branchToBranch [] = []
branchToBranch (t:ts) =
    case t of
        Branch exprs -> [Branch (map (branchToBranch . (++ ts)) exprs)]
        Detour exprs -> Detour (map branchToBranch exprs) : branchToBranch ts
        _            -> t : branchToBranch ts

normalize :: Expr -> Expr
normalize = branchToBranch . detourToBranch

-- "Dumb" depth, assumes there are no cycles
depth :: Expr -> Integer
depth []             = 0
depth [Branch exprs] = maximum $ map depth exprs
depth (Step _:ts)    = 1 + depth ts
depth _              = error "Un-normalized expression"

type Position = (Integer, Integer)

move :: Direction -> Position -> Position
move d (x, y) =
    case d of
        N -> (x, y - 1)
        S -> (x, y + 1)
        E -> (x + 1, y)
        W -> (x - 1, y)

-- Validation
hasCycles :: Expr -> Bool
hasCycles = go (0, 0) $ S.fromList [(0, 0)]
  where
    go pos set [] = False
    go pos set [Branch exprs] = any (go pos set) exprs -- this is wrong, needs to be a fold as we have to "thread" the set together, not just give all a copy
    go pos set (Step d:ts) =
        let newpos = move d pos
         in newpos `S.member` set || go newpos (S.insert newpos set) ts
    go pos set _ = error "Un-normalized expression"

type Graph = M.Map Position [Position]

buildGraph :: Expr -> Graph
buildGraph = go (0, 0) $ M.fromList [((0, 0), [])]
  where
    go :: Position -> Graph -> Expr -> Graph
    go pos gr [] = gr
    go pos gr [Branch exprs] = foldl (go pos) gr exprs
    go pos gr (Step d:ts) =
        let newpos = move d pos
         in go newpos (edge pos newpos . edge newpos pos $ gr) ts
    go pos gr _ = error "Un-normalized expression"
    edge a b = M.insertWith (((.) . (.)) nub (++)) a [b]

bfsDepth :: Graph -> Position -> Integer
bfsDepth gr pos = go 0 gr S.empty [(0, pos)]
  where
    go :: Integer -> Graph -> S.Set Position -> [(Integer, Position)] -> Integer
    go d gr set [] = d
    go d gr set ((pd, p):ps) =
        if p `S.notMember` set
            then let ns = zip (repeat (pd + 1)) $ M.findWithDefault [] p gr
                  in go (max d pd) gr (S.insert p set) (ps ++ ns)
            else go d gr set ps

bfsDepthAtLeast1000 :: Graph -> Position -> Integer
bfsDepthAtLeast1000 gr pos = go 0 gr S.empty [(0, pos)]
  where
    go :: Integer -> Graph -> S.Set Position -> [(Integer, Position)] -> Integer
    go c gr set [] = c
    go c gr set ((pd, p):ps) =
        if p `S.notMember` set
            then let ns = zip (repeat (pd + 1)) $ M.findWithDefault [] p gr
                     nc =
                         if pd >= 1000
                             then c + 1
                             else c
                  in go nc gr (S.insert p set) (ps ++ ns)
            else go c gr set ps

main :: IO ()
main = do
    input <- parseInput <$> loadInput
    -- print $ hasEmptyExprs input
    -- print $ hasTermsAfterBranch input
    -- print $ hasBranches input
    -- print $ hasDetours input
    let fixed = normalize input
    -- print $ hasBranches fixed
    -- print $ hasDetours fixed
    -- print $ depth fixed
    -- print $ hasCycles fixed
    let graph = buildGraph fixed
    print $ bfsDepth graph (0, 0)
    print $ bfsDepthAtLeast1000 graph (0, 0)
