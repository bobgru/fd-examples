{-
From David Overton:
https://overtond.blogspot.com/2008/07/haskell-sudoku-solver-using-finite.html
-}

module Sudoku (Puzzle, printSudoku, displayPuzzle, sudoku, 
               chunk, transpose) where

import Control.Monad
import Data.List (transpose)
import FD

type Puzzle = [Int]

displayPuzzle :: Puzzle -> String
displayPuzzle = unlines . map show . chunk 9

printSudoku :: Puzzle -> IO ()
printSudoku = putStr . unlines . map displayPuzzle . sudoku

chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk n xs = ys : chunk n zs where
    (ys, zs) = splitAt n xs

sudoku :: Puzzle -> [Puzzle]
sudoku puzzle = runFD $ do
    vars <- newVars 81 [1..9]
    zipWithM_ (\x n -> when (n > 0) (x `hasValue` n)) vars puzzle
    mapM_ allDifferent (rows vars)
    mapM_ allDifferent (columns vars)
    mapM_ allDifferent (boxes vars)
    labelling vars

rows, columns, boxes :: [a] -> [[a]]
rows = chunk 9
columns = transpose . rows
boxes = concat . map (map concat . transpose) . chunk 3 . chunk 3 . chunk 3

