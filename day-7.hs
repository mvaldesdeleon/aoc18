{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}

import           Control.Monad
import           Control.Monad.Loops
import           Control.Monad.State
import           Data.List
import qualified Data.Map.Strict     as M
import           Data.Maybe
import           Text.Parsec         (Parsec, char, many, optional, parse,
                                      string, upper)

loadFile :: IO String
loadFile = readFile "inputs/day-7.txt"

newtype Dependency =
    Dependency (Char, Char)
    deriving (Show)

-- Parsing
dependency :: Parsec String () Dependency
dependency =
    mkDependency <$>
    (string "Step " *> upper <* string " must be finished before step ") <*>
    (upper <* string " can begin.") <*
    optional (char '\n')
  where
    mkDependency a b = Dependency (a, b)

parseInput :: String -> [Dependency]
parseInput input =
    case result of
        Left e             -> error $ show e
        Right dependencies -> dependencies
  where
    result = parse (many dependency) "" (prepare input)
    prepare = unlines . nub . sort . lines

-- Problem
order :: [Dependency] -> String
order deps = evalState (topSort deps) tsEmpty

data TopSort = TopSort
    { next     :: [Char]
    , incoming :: M.Map Char Integer
    , outgoing :: M.Map Char [Char]
    } deriving (Show)

tsEmpty :: TopSort
tsEmpty = TopSort [] M.empty M.empty

tsInit :: [Dependency] -> State TopSort ()
tsInit deps = do
    mapM_ tsInitStep deps
    tsInitNext

tsInitStep :: Dependency -> State TopSort ()
tsInitStep (Dependency (d, p)) = do
    accountFor d
    increaseIncoming p
    addOutgoing d p
  where
    accountFor d =
        modify
            (\ts@TopSort {..} -> ts {incoming = M.insertWith (+) d 0 incoming})
    increaseIncoming p =
        modify
            (\ts@TopSort {..} -> ts {incoming = M.insertWith (+) p 1 incoming})
    addOutgoing d p =
        modify
            (\ts@TopSort {..} ->
                 ts {outgoing = M.insertWith (flip (++)) d [p] outgoing})

tsInitNext :: State TopSort ()
tsInitNext =
    modify
        (\ts@TopSort {..} ->
             ts {next = next ++ M.foldlWithKey noIncoming [] incoming})
  where
    noIncoming next p count =
        if count == 0
            then next ++ [p]
            else next

topSort :: [Dependency] -> State TopSort String
topSort deps = do
    tsInit deps
    whileM hasNext topSortStep
  where
    hasNext = do
        ns <- gets next
        return $ not (null ns)

topSortStep :: State TopSort Char
topSortStep = do
    n <- popNext
    next <- popOutgoing n
    mapM_ subtractIncoming next
    return n
  where
    popNext = do
        (n:ns) <- gets next
        modify (\ts@TopSort {..} -> ts {next = ns})
        return n
    popOutgoing n = do
        next <- gets (M.lookup n . outgoing)
        modify (\ts@TopSort {..} -> ts {outgoing = M.delete n outgoing})
        return $ fromMaybe [] next
    subtractIncoming n = do
        (Just c) <- gets (M.lookup n . incoming)
        modify (\ts@TopSort {..} -> ts {incoming = M.insert n (c - 1) incoming})
        when (c == 1) $ modify (\ts@TopSort {..} -> ts {next = sort (n : next)})

main :: IO ()
main = do
    input <- parseInput <$> loadFile
    print $ order input
