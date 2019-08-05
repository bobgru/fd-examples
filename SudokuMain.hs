{-
Compile with:
    ghc --make SudokuMain

From David Overton:
https://overtond.blogspot.com/2008/07/haskell-sudoku-solver-using-finite.html
-}

module Main where

import Sudoku
import Console
main = do putStrLn (displayPuzzle hardest)
          printSudoku hardest

hardest :: Puzzle
hardest = [
    8, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 3, 6, 0, 0, 0, 0, 0,
    0, 7, 0, 0, 9, 0, 2, 0, 0,
    0, 5, 0, 0, 0, 7, 0, 0, 0,
    0, 0, 0, 0, 4, 5, 7, 0, 0,
    0, 0, 0, 1, 0, 0, 0, 3, 0,
    0, 0, 1, 0, 0, 0, 0, 6, 8,
    0, 0, 8, 5, 0, 0, 0, 1, 0,
    0, 9, 0, 0, 0, 0, 4, 0, 0 ]

