{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}

-- From David Overton, modified:
-- https://overtond.blogspot.com/2008/07/pre.html

module FD (
    -- Types
    FD,            -- Monad for finite domain constraint solver
    FDVar,         -- Finite domain solver variable

    -- Functions
    runFD,         -- Run the monad and return a list of solutions.
    newVar,        -- Create a new FDVar
    newVars,       -- Create multiple FDVars
    hasValue,      -- Constrain a FDVar to a specific value
    hasValues,     -- Constrain a FDVar to set of values
    avoids,        -- Constrain a FDVar to exclude a set of values
    same,          -- Constrain two FDVars to be the same
    relatedBy,     -- Constrain two FDVars by predicate on values
    relatedBy3,    -- Constrain three FDVars by predicate on values
    relatedBy4,    -- Constrain four FDVars by predicate on values
    allRelatedBy,  -- Constrain list of FDVars pairwise by predicate
    relatedBy',    -- Constrain two FDVars by predicate on index-value pairs
    allRelatedBy', -- Constrain list of FDVars pairwise by predicate
    different,     -- Constrain two FDVars to be different
    allDifferent,  -- Constrain a list of FDVars to be different
    sumOf,         -- Constrain a list of FDVars to a sum
    (.<.),         -- Constrain one FDVar to be less than another
    labelling,     -- Backtracking search for all solutions
    getDomains
    ) where

import Prelude hiding (lookup)
import Control.Monad.State.Lazy
import Control.Monad.Trans
import qualified Data.IntSet as IntSet
import Data.IntSet (IntSet)
import Data.List (elemIndex, nub, sort, (\\))
import qualified Data.Map as Map
import Data.Map ((!), Map)
import Data.Maybe (fromJust, isJust)

-- The FD monad
newtype FD s a = FD { unFD :: StateT (FDState s) [] a }
    deriving (Monad, MonadPlus, MonadState (FDState s))

-- FD variables
newtype FDVar s = FDVar { unFDVar :: Int } deriving (Ord, Eq)

type VarSupply s = FDVar s
data VarInfo s = VarInfo
     { delayedConstraints :: FD s (), values :: IntSet }
type VarMap s = Map (FDVar s) (VarInfo s)
data FDState s = FDState
     { varSupply :: VarSupply s, varMap :: VarMap s }

-- Run the FD monad and produce a lazy list of possible solutions.
runFD :: (forall s . FD s a) -> [a]
runFD fd = evalStateT (unFD fd) initState

initState :: FDState s
initState = FDState { varSupply = FDVar 0, varMap = Map.empty }

-- Get a new FDVar
newVar :: [Int] -> FD s (FDVar s)
newVar domain= do
    v <- nextVar
    v `isOneOf` domain
    return v
    where
        nextVar :: FD s (FDVar s)
        nextVar = do
            s <- get
            let v = varSupply s
            put $ s { varSupply = FDVar (unFDVar v + 1) }
            return v
        isOneOf :: FDVar s -> [Int] -> FD s ()
        x `isOneOf` domain=
            modify $ \s ->
                let vm = varMap s
                    vi = VarInfo {
                        delayedConstraints = return (),
                        values = IntSet.fromList domain}
                in
                s { varMap = Map.insert x vi vm }

newVars :: Int -> [Int] -> FD s [FDVar s]
newVars n domain = replicateM n (newVar domain)

-- Lookup the current domain of a variable.
lookup :: FDVar s -> FD s IntSet
lookup x = do
    s <- get
    return . values $ varMap s ! x

-- Update the domain of a variable and fire all delayed constraints
-- associated with that variable.
update :: FDVar s -> IntSet -> FD s ()
update x i = do
    s <- get
    let vm = varMap s
    let vi = vm ! x
    put $ s { varMap = Map.insert x (vi { values = i}) vm }
    delayedConstraints vi

-- Add a new constraint for a variable to the constraint store.
addConstraint :: FDVar s -> FD s () -> FD s ()
addConstraint x constraint = do
    s <- get
    let vm = varMap s
    let vi = vm ! x
    let cs = delayedConstraints vi
    put $ s { varMap =
        Map.insert x (vi { delayedConstraints = cs >> constraint }) vm }
 
-- Useful helper function for adding binary constraints between FDVars.
type BinaryConstraint s = FDVar s -> FDVar s -> FD s ()
addBinaryConstraint :: BinaryConstraint s -> BinaryConstraint s
addBinaryConstraint f x y = do
    let constraint  = f x y
    constraint
    addConstraint x constraint
    addConstraint y constraint

type MultiConstraint s = [FDVar s] -> FD s ()
addMultiConstraint :: MultiConstraint s -> MultiConstraint s
addMultiConstraint f xs = do
    let constraint  = f xs
    constraint
    mapM_ (\x -> addConstraint x constraint) xs

-- Constrain a variable to a particular value.
hasValue :: FDVar s -> Int -> FD s ()
var `hasValue` val = do
    vals <- lookup var
    guard $ val `IntSet.member` vals
    let i = IntSet.singleton val
    when (i /= vals) $ update var i

-- Constrain a variable to a particular set of values.
hasValues :: FDVar s -> [Int] -> FD s ()
var `hasValues` vals = do
    vs <- lookup var
    let i  = IntSet.fromList vals
        i' = IntSet.intersection vs (IntSet.fromList vals)
    guard $ not $ IntSet.null i'
    when (i' /= vs) $ update var i'

-- Constrain a variable to a particular set of values.
doesNotHaveValues :: FDVar s -> [Int] -> FD s ()
doesNotHaveValues = avoids

avoids :: FDVar s -> [Int] -> FD s ()
var `avoids` vals = do
    vs <- lookup var
    let i  = IntSet.fromList vals
        i' = IntSet.difference vs (IntSet.fromList vals)
    guard $ not $ IntSet.null i'
    when (i' /= vs) $ update var i'

-- Constrain two variables to have the same value.
same :: FDVar s -> FDVar s -> FD s ()
same = addBinaryConstraint $ \x y -> do
    xv <- lookup x
    yv <- lookup y
    let i = IntSet.intersection xv yv
    guard $ not $ IntSet.null i
    when (i /= xv) $ update x i
    when (i /= yv) $ update y i

-- Constrain two variables to have different values.
different :: FDVar s -> FDVar s -> FD s ()
different = addBinaryConstraint $ \x y -> do
    xv <- lookup x
    yv <- lookup y
    guard $ IntSet.size xv > 1 || IntSet.size yv > 1 || xv /= yv
    when (IntSet.size xv == 1 && xv `IntSet.isProperSubsetOf` yv) $
        update y (yv `IntSet.difference` xv)
    when (IntSet.size yv == 1 && yv `IntSet.isProperSubsetOf` xv) $
        update x (xv `IntSet.difference` yv)

-- Constrain a list of variables to all have different values.
allDifferent :: [FDVar s] -> FD s ()
allDifferent (x:xs) = do
    mapM_ (different x) xs
    allDifferent xs
allDifferent _ = return ()

-- Constrain two variables to be related by a predicate on values.
-- Enumerates all pairs and filters by the predicate.
--
-- This won't scale so adding an application-specific constraint
-- would be necessary for problems of a certain size.
relatedBy :: (Int -> Int -> Bool) -> FDVar s -> FDVar s -> FD s ()
relatedBy f = addBinaryConstraint $ \x y -> do
    xv <- lookup x
    yv <- lookup y
    let xs = IntSet.toList xv
        ys = IntSet.toList yv
        ps = [ (x, y) | x <- xs, y <- ys, f x y ]
        xs' = map fst ps
        ys' = map snd ps
        xv' = IntSet.fromList xs'
        yv' = IntSet.fromList ys'
    guard $ not $ null ps
    when (xv /= xv') $ update x xv'
    when (yv /= yv') $ update y yv'

-- This won't scale so adding an application-specific constraint
-- would be necessary for problems of a certain size.
relatedBy3 :: ([Int] -> Bool) -> [FDVar s] -> FD s ()
relatedBy3 f = addMultiConstraint $ \vs -> do
    let [x, y, z] = vs
    xv <- lookup x
    yv <- lookup y
    zv <- lookup z
    let xs = IntSet.toList xv
        ys = IntSet.toList yv
        zs = IntSet.toList zv
        ts = [ (x', y', z') | x' <- xs, y' <- ys, z' <- zs, f [x', y', z'] ]
        xs' = map (\(a, _, _) -> a) ts
        ys' = map (\(_, a, _) -> a) ts
        zs' = map (\(_, _, a) -> a) ts
        xv' = IntSet.fromList xs'
        yv' = IntSet.fromList ys'
        zv' = IntSet.fromList zs'
    guard $ not $ null ts
    when (xv /= xv') $ update x xv'
    when (yv /= yv') $ update y yv'
    when (zv /= zv') $ update z zv'

-- This won't scale so adding an application-specific constraint
-- would be necessary for problems of a certain size.
relatedBy4 :: ([Int] -> Bool) -> [FDVar s] -> FD s ()
relatedBy4 f = addMultiConstraint $ \vs -> do
    let [x, y, z, w] = vs
    xv <- lookup x
    yv <- lookup y
    zv <- lookup z
    wv <- lookup w
    let xs = IntSet.toList xv
        ys = IntSet.toList yv
        zs = IntSet.toList zv
        ws = IntSet.toList wv
        qs = [ (x', y', z', w') | x' <- xs, y' <- ys, z' <- zs, w' <- ws, f [x', y', z', w'] ]
        xs' = map (\(a, _, _, _) -> a) qs
        ys' = map (\(_, a, _, _) -> a) qs
        zs' = map (\(_, _, a, _) -> a) qs
        ws' = map (\(_, _, _, a) -> a) qs
        xv' = IntSet.fromList xs'
        yv' = IntSet.fromList ys'
        zv' = IntSet.fromList zs'
        wv' = IntSet.fromList ws'
    guard $ not $ null qs
    when (xv /= xv') $ update x xv'
    when (yv /= yv') $ update y yv'
    when (zv /= zv') $ update z zv'
    when (wv /= wv') $ update w wv'

-- Constrain list of variables to be pairwise related by a predicate 
-- on values. For each pair of variables, enumerates all pairs of 
-- values and filters by the predicate.
--
-- This won't scale so adding an application-specific constraint
-- would be necessary for problems of a certain size.
allRelatedBy :: (Int -> Int -> Bool) -> [FDVar s] -> FD s ()
allRelatedBy f (x:xs) = do
    mapM_ (relatedBy f x) xs
    allRelatedBy f xs
allRelatedBy _ _ = return ()

-- Constrain two variables to be related by a predicate on index-value
-- pairs. Enumerates all index-value pairs and filters by the predicate.
--
-- This won't scale so adding an application-specific constraint
-- would be necessary for problems of a certain size.
relatedBy' :: ((Int, Int) -> (Int, Int) -> Bool) -> [FDVar s] -> FDVar s -> FDVar s -> FD s ()
relatedBy' f vars = addBinaryConstraint $ \x y -> do
    xv <- lookup x
    yv <- lookup y
    let xpos  = x `elemIndex` vars 
        ypos  = y `elemIndex` vars
        xpos' = fromJust xpos
        ypos' = fromJust ypos
        xs    = IntSet.toList xv
        ys    = IntSet.toList yv
        ps    = [ ((xpos', x), (ypos', y)) |
                  x <- xs, y <- ys, f (xpos', x) (ypos', y) ]
        xs'   = map (snd . fst) ps
        ys'   = map (snd . snd) ps
        xv'   = IntSet.fromList xs'
        yv'   = IntSet.fromList ys'
    guard $ isJust xpos && isJust ypos
    guard $ not $ null ps
    when (xv /= xv') $ update x xv'
    when (yv /= yv') $ update y yv'

-- Constrain list of variables to be pairwise related by a predicate 
-- on index-value pairs. For each pair of variables, enumerates all 
-- pairs of index and value and filters by the predicate.
-- This won't scale so adding an application-specific constraint
-- would be necessary for problems of a certain size.
allRelatedBy' :: ((Int, Int) -> (Int, Int) -> Bool) -> [FDVar s]  -> [FDVar s] -> FD s ()
allRelatedBy' f vars (x:xs) = do
    mapM_ (relatedBy' f vars x) xs
    allRelatedBy' f vars xs
allRelatedBy' _  _ _ = return ()

isSingleton :: IntSet -> Bool
isSingleton v = 1 == IntSet.size v

isSingletonVar :: FDVar s -> FD s Bool
isSingletonVar x = lookup x >>= return . isSingleton

sumSingletons :: [IntSet] -> Int
sumSingletons = sum . map (head . IntSet.toList)

sumVars :: [FDVar s] -> FD s Int
sumVars xs = mapM lookup xs >>= return . sumSingletons

subsets []     = [[]]
subsets (x:xs) = (map (x :) $ subsets xs) ++ subsets xs

sumSets :: Int -> Int -> [[Int]]
sumSets n len = filter ((n ==) . sum)
              . filter ((len ==) . length)
              $ subsets [1..9]

sumSetDomain :: Int -> Int -> IntSet
sumSetDomain n len = IntSet.fromList
                   . nub
                   . sort
                   . concat
                   $ sumSets n len

limitToSet :: IntSet -> FDVar s -> FD s ()
limitToSet xs x = do v <- lookup x
                     let i = IntSet.intersection xs v
                     guard $ not $ IntSet.null i
                     when (i /= v) $ update x i

sumOf' :: Int -> [FDVar s] -> FD s ()
sumOf' n ws = do
    case ws of
      []  -> guard $ n == 0
      [x] -> do
               guard $ n > 0
               v <- lookup x
               let i = IntSet.intersection v (IntSet.singleton n) 
               guard $ not $ IntSet.null i
               when (i /= v) $ update x i
      xs  -> do
               guard $ n > 0
               zs <- filterM isSingletonVar xs
               if (not . null) zs
               then do
                   n' <- sumVars zs
                   sumOf' (n - n') (xs \\ zs)
               else do
                   let ss = sumSetDomain n (length xs)
                   vs <- mapM lookup xs
                   guard $ all (not . IntSet.null)
                         $ map (IntSet.intersection ss) vs
                   mapM_ (limitToSet ss) xs
 

-- Constrain a list of variables to have a sum.
sumOf :: Int -> [FDVar s] -> FD s ()
n `sumOf` vars = addMultiConstraint (n `sumOf'`) vars

-- Constrain one variable to have a value less than the value of another
-- variable.
(.<.) :: FDVar s -> FDVar s -> FD s ()
(.<.) = addBinaryConstraint $ \x y -> do
    xv <- lookup x
    yv <- lookup y
    let xv' = IntSet.filter (< IntSet.findMax yv) xv
    let yv' = IntSet.filter (> IntSet.findMin xv) yv
    guard $ not $ IntSet.null xv'
    guard $ not $ IntSet.null yv'
    when (xv /= xv') $ update x xv'
    when (yv /= yv') $ update y yv'

-- Label variables using a depth-first left-to-right search.
labelling :: [FDVar s] -> FD s [Int]
labelling = mapM label where
    label var = do
        vals <- lookup var
        val <- FD . lift $ IntSet.toList vals
        var `hasValue` val
        return val

getDomains :: [FDVar s] -> FD s [[Int]]
getDomains = mapM label where
    label var = do
        vals <- lookup var
        return $ IntSet.toList vals

