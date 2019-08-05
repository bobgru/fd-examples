-- ghci LotAllocation.hs
-- printAnswer

module LotAllocation ( Answer
                     , printAnswer
                     , displayAnswer)
where

import Control.Monad
import Data.List ((\\))
import FD

type Answer = [Int]

displayAnswer :: Answer -> String
displayAnswer [rec,apts,houses,cemetery,dump] =
    unlines
    [ "recreation = " ++ show rec
    , "apartments = " ++ show apts
    , "houses     = " ++ show houses
    , "cemetery   = " ++ show cemetery
    , "dump       = " ++ show dump]
displayAnswer _ = error "bad argument"

printAnswerWith :: (Answer -> String) -> IO ()
printAnswerWith f = return solve >>= mapM_ (putStrLn . f)

printAnswer :: IO ()
printAnswer = printAnswerWith displayAnswer

linesOfSight :: [(Int, Int)]
linesOfSight = [
  (1, 2), (2, 3), (3, 4), (3, 7),
  (4, 8), (5, 6), (6, 7), (7, 8)
  ]

notVisibleFrom :: Int -> Int -> Bool
notVisibleFrom x y =
  let (x', y') = if x < y then (x, y) else (y, x)
  in (x', y') `notElem` linesOfSight

solve :: [Answer]
solve = runFD $ do
    vars@[rec,apts,houses,cemetery,dump] <- newVars 5 [1..8]
    allDifferent vars
    rec `hasValues` [2,3,4]
    mapM_ (flip avoids [3,4,7,8]) (vars \\ [rec])
    mapM_ (flip avoids [3,4]) [apts, houses]
    mapM_ (flip avoids [1,2]) [rec, apts, houses]
    dump `avoids` [2,3,4]
    mapM_ (relatedBy notVisibleFrom dump) [apts, houses]
    labelling vars

