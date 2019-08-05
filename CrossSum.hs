module CrossSum ( Puzzle
                , PuzzleSpec(..)
                , CrossSum(..)
                , printCrossSum
                , displayPuzzle
                , crossSum)
where

import           Control.Monad
import           Data.List (intercalate, nub, sort)
import qualified Data.Map as M
import           Data.Maybe (fromJust)
import           FD

data PuzzleSpec =
  PuzzleSpec {
      rows :: [CrossSum]
    , cols :: [CrossSum]
  } deriving (Show, Eq)

cells :: PuzzleSpec -> [Cell]
cells ps = nub . sort . concat $ rows' ++ cols'
  where
    rows' = map mkRowCells $ rows ps
    cols' = map mkColCells $ cols ps

mkRowCells :: CrossSum -> [Cell]
mkRowCells (CrossSum (r, c) l s) = [ (r, c') | c' <- [c..c+l-1]]

mkColCells :: CrossSum -> [Cell]
mkColCells (CrossSum (r, c) l s) = [ (r', c) | r' <- [r..r+l-1]]

type Puzzle = M.Map Cell Int

type Cell = (Int, Int)
data CrossSum = 
  CrossSum {
      csCell   :: Cell
    , csLength :: Int
    , csSum    :: Int
  } deriving (Show, Eq)


displayPuzzle :: Puzzle -> String
displayPuzzle p = unlines
                . map (intercalate " " . map show')
                . chunk (1 + maxCol p)
                $ digits p

maxRow :: Puzzle -> Int
maxRow = maximum . map fst . M.keys

maxCol :: Puzzle -> Int
maxCol = maximum . map snd . M.keys

show' :: Int -> String
show' x = if x == 0 then " " else show x 

digits :: Puzzle -> [Int]
digits p = [ maybe 0 id ms | r <- [0..maxRow p]
                           , c <- [0..maxCol p]
                           , let ms = M.lookup (r, c) p ]

chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk n xs = ys : chunk n zs where
    (ys, zs) = splitAt n xs

printCrossSum :: PuzzleSpec -> IO ()
printCrossSum = putStr . unlines . map displayPuzzle . crossSum

puzzleFromAnswer :: [Cell] -> [Int] -> Puzzle
puzzleFromAnswer cs ss = M.fromList $ zip cs ss

crossSum :: PuzzleSpec -> [Puzzle]
crossSum ps = runFD $ do
     
    -- get cells from ps
    let allCells = cells ps
        rowCells = map mkRowCells $ rows ps
        colCells = map mkColCells $ cols ps

    vars <- newVars (length allCells) [1..9]

    let 
        -- cellToVarMap :: M.Map Cell (FDVar s)
        cellToVarMap = M.fromList (zip allCells vars)
        -- varFromCell :: Cell -> FDVar s
        varFromCell c = fromJust $ M.lookup c cellToVarMap 

        rowVars = map (map varFromCell) rowCells
        colVars = map (map varFromCell) colCells

    -- All cells in each row and column are different.
    mapM_ allDifferent rowVars
    mapM_ allDifferent colVars

    -- The sums in each row and column must be correct.
    zipWithM_ sumOf (map csSum $ rows ps) rowVars
    zipWithM_ sumOf (map csSum $ cols ps) colVars

    answer <- labelling vars
    return $ puzzleFromAnswer allCells answer

