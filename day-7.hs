{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE RecordWildCards  #-}

import           Control.Monad
import           Control.Monad.Loops
import           Control.Monad.State
import           Data.Char
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
order deps = evalState (topSort deps) (tsEmpty 0)

data Worker
    = Free
    | Busy Char
           Integer
    deriving (Show)

instance Eq Worker where
    Free == Free = True
    (Busy _ a) == (Busy _ b) = a == b
    _ == _ = False

isFree :: Worker -> Bool
isFree Free = True
isFree _    = False

isBusy :: Worker -> Bool
isBusy = not . isFree

isDone :: Worker -> Bool
isDone (Busy _ 0) = True
isDone _          = False

remaining :: Worker -> Integer
remaining (Busy _ w) = w

data TopSort = TopSort
    { next     :: [Char]
    , incoming :: M.Map Char Integer
    , outgoing :: M.Map Char [Char]
    , workers  :: [Worker]
    } deriving (Show)

tsEmpty :: Integer -> TopSort
tsEmpty workers = TopSort [] M.empty M.empty (Free <$ [1 .. workers])

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
    removeNode n
    return n
  where
    popNext = do
        (n:ns) <- gets next
        modify (\ts@TopSort {..} -> ts {next = ns})
        return n

removeNode :: Char -> State TopSort ()
removeNode n = do
    next <- popOutgoing n
    mapM_ subtractIncoming next
  where
    popOutgoing n = do
        next <- gets (M.lookup n . outgoing)
        modify (\ts@TopSort {..} -> ts {outgoing = M.delete n outgoing})
        return $ fromMaybe [] next
    subtractIncoming n = do
        (Just c) <- gets (M.lookup n . incoming)
        modify (\ts@TopSort {..} -> ts {incoming = M.insert n (c - 1) incoming})
        when (c == 1) $ modify (\ts@TopSort {..} -> ts {next = sort (n : next)})

totalTime :: Integer -> [Dependency] -> Integer
totalTime workers deps = evalState (topSortTime deps) (tsEmpty workers)

topSortTime :: [Dependency] -> State TopSort Integer
topSortTime deps = do
    tsInit deps
    sum <$> whileM hasNextOrWorking topSortTimeStep
  where
    hasNextOrWorking = do
        ns <- gets next
        ws <- gets (filter isBusy . workers)
        return $ not (null ns) || not (null ws)

topSortTimeStep :: State TopSort Integer
topSortTimeStep = do
    whileM_ hasNextAndNotWorking assignNext
    wk <- gets (minimum . map remaining . filter isBusy . workers)
    doWork wk
    doneWorkers <- gets (elemIndices (Busy '?' 0) . workers)
    mapM_ cleanupWorker doneWorkers
    return wk
  where
    hasNextAndNotWorking = do
        ns <- gets next
        ws <- gets (filter isFree . workers)
        return $ not (null ns) && not (null ws)
    assignNext = do
        (n:ns) <- gets next
        modify (\ts@TopSort {..} -> ts {next = ns, workers = assign n workers})
    assign n ws =
        case elemIndex Free ws of
            Just i -> replace (fromIntegral i) (Busy n (duration n)) ws
    doWork wk = modify (\ts@TopSort {..} -> ts {workers = work wk workers})
    work :: Integer -> [Worker] -> [Worker]
    work wk =
        map
            (\case
                 Busy n w -> Busy n (w - wk)
                 Free -> Free)
    cleanupWorker i = do
        (Busy n _) <- gets (flip (!!) i . workers)
        removeNode n
        modify
            (\ts@TopSort {..} ->
                 ts {workers = replace (fromIntegral i) Free workers})

replace :: Integer -> a -> [a] -> [a]
replace i v (a:as) =
    if i == 0
        then v : as
        else a : replace (i - 1) v as
replace _ _ [] = []

duration :: Char -> Integer
duration c = fromIntegral (ord c - ord 'A' + 61)

main :: IO ()
main = do
    input <- parseInput <$> loadFile
    print $ order input
    print $ totalTime 5 input
