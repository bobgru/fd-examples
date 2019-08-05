module NQueens where

import Control.Monad
import FD

printQueens :: Int -> IO ()
printQueens = putStr . unlines . map show . nqueens

notOnDiag :: (Int, Int) -> (Int, Int) -> Bool
notOnDiag (q1, r1) (q2, r2) = abs (q1 - q2) /= abs (r1 - r2)

nqueens :: Int -> [[Int]]
nqueens n = runFD $ do
    vars <- newVars n [1..n]
    allDifferent vars
    allRelatedBy' notOnDiag vars vars 
    labelling vars
