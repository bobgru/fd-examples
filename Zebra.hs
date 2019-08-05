-- ghci Zebra.hs
-- printAnswer

module Zebra ( Answer
             , printAnswer
             , displayAnswer)
where

import Data.List (sortBy)
import FD

-- Values are house numbers which correlate attributes.
type Answer = [Int]

-- Indexes to attribute variables
colors        = [0..4]
nationalities = [5..9]
pets          = [10..14]
drinks        = [15..19]
cigarettes    = [20..24]

names :: [String]
names =
    [ "red"
    , "green"
    , "ivory"
    , "yellow"
    , "blue"

    , "Engishwoman"
    , "Spaniard"
    , "Ukrainian"
    , "Norwegian"
    , "Japanese"

    , "dog"
    , "snail"
    , "fox"
    , "horse"
    , "zebra"

    , "coffee"
    , "tea"
    , "milk"
    , "orange juice"
    , "water"

    , "Oldgold"
    , "Kools"
    , "Chesterfield"
    , "Lucky Strike"
    , "Parliament"]

displayAnswer :: Answer -> String
displayAnswer xs = unlines $ map showHouse [0..4]
  where 
    pairWithName :: Int -> (Int, String)
    pairWithName v = (xs !! v, names !! v) 
    orderByVar :: [Int] -> [String]
    orderByVar = map snd . sortBy cmp . map pairWithName
    cmp :: Ord a => (a, b) -> (a, c) -> Ordering
    cmp (x, s) (y, t) = compare x y

    cs = orderByVar colors
    ns = orderByVar nationalities
    ps = orderByVar pets
    ds = orderByVar drinks
    gs = orderByVar cigarettes

    showHouse h =
      "The " ++ (ns !! h) ++
      " lives in the " ++ (cs !! h) ++ " house " ++
      " with the " ++ (ps !! h) ++ ", " ++
      " drinks " ++ (ds !! h) ++ ", " ++
      " and smokes " ++ (gs !! h)

printAnswer :: IO ()
printAnswer = return solve >>= mapM_ (putStrLn . displayAnswer)

chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk n xs = ys : chunk n zs where
    (ys, zs) = splitAt n xs

nextTo :: Int -> Int -> Bool
x `nextTo` y = abs (x - y) == 1

hardRightOf :: Int -> Int -> Bool
x `hardRightOf` y = x == y + 1

solve :: [Answer]
solve = runFD $ do
    vars <- newVars 25 [0..4]

    let [englishwoman, spaniard, ukrainian, norwegian, japanese] =
            map (vars !!) nationalities
        [red, green, ivory, yellow, blue] =
            map (vars !!) colors
        [dog, snail, fox, horse, zebra] =
            map (vars !!) pets
        [coffee, tea, milk, orangeJuice, water] =
            map (vars !!) drinks
        [oldgold, kools, chesterfield, luckyStrike, parliament] =
            map (vars !!) cigarettes

    mapM_ allDifferent $ (chunk 5 vars)
    englishwoman `same` red
    spaniard `same` dog
    coffee `same` green
    ukrainian `same` tea
    relatedBy hardRightOf green ivory
    oldgold `same` snail
    kools `same` yellow
    milk `hasValue` 2
    norwegian `hasValue` 0
    relatedBy nextTo chesterfield fox
    relatedBy nextTo yellow horse
    luckyStrike `same` orangeJuice   
    japanese `same` parliament
    relatedBy nextTo norwegian blue

    labelling vars

-- Was useful for debugging
-- The solution was good but the display was wrongly interpreted.
-- It was interesting to see how quickly the solution was found.

displayDomains :: [[Int]] -> String
displayDomains = unlines . map format . zip names 
  where
    format :: (String, [Int]) -> String
    format (n, xs) = n ++ (pad (len + 1) $ " = " ++ show xs)
    pad :: Int -> String -> String
    pad w s = (concat . take (w - len) $ repeat " ") ++ s
    len :: Int
    len = maximum (map length names)

printDomains :: IO ()
printDomains = return solveDomains >>= mapM_ (putStrLn . displayDomains)

solveDomains :: [[[Int]]]
solveDomains = runFD $ do
    vars <- newVars 25 [0..4]

    let [englishwoman, spaniard, ukrainian, norwegian, japanese] =
            map (vars !!) nationalities
        [red, green, ivory, yellow, blue] =
            map (vars !!) colors
        [dog, snail, fox, horse, zebra] =
            map (vars !!) pets
        [coffee, tea, milk, orangeJuice, water] =
            map (vars !!) drinks
        [oldgold, kools, chesterfield, luckyStrike, parliament] =
            map (vars !!) cigarettes

    mapM_ allDifferent $ (chunk 5 vars)
    englishwoman `same` red
    spaniard `same` dog
    coffee `same` green
    ukrainian `same` tea
    relatedBy hardRightOf green ivory
    oldgold `same` snail
    kools `same` yellow
    milk `hasValue` 2
    norwegian `hasValue` 0
    relatedBy nextTo chesterfield fox
    relatedBy nextTo yellow horse
    luckyStrike `same` orangeJuice   
    japanese `same` parliament
    relatedBy nextTo norwegian blue

    -- follow depth-first search

    red `hasValue` 2
    green `hasValue` 4
    ivory `hasValue` 3
    yellow `hasValue` 0
    blue `hasValue` 1

    englishwoman `hasValue` 2
    spaniard `hasValue` 3
   
    -- now have all singletons with answer 

    -- ukrainian `hasValue` 1
    -- norwegian `hasValue` 0
    -- japanese `hasValue` 4

    -- dog `hasValue` 3
    -- snail `hasValue` 2
    -- fox `hasValue` 0
    -- horse `hasValue` 1
    -- zebra `hasValue` 4

    -- coffee `hasValue` 4
    -- tea `hasValue` 1
    -- milk `hasValue` 2
    -- orangeJuice `hasValue` 3
    -- water `hasValue` 0

    -- oldgold `hasValue` 2
    -- kools `hasValue` 0
    -- chesterfield `hasValue` 1
    -- luckyStrike `hasValue` 3
    -- parliament `hasValue` 4

    getDomains vars

