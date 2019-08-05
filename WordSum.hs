-- ghci WordSum.hs
-- printAnswer

module WordSum( Answer
             , printAnswer
             , displayAnswer)
where

import FD

-- Values assigned to letters to make sum work.
type Answer = [Int]

displayAnswer :: Answer -> String
displayAnswer xs =
  let [d, e, m, n, o, r, s, y] = map show $ take (length letters) xs
  in  unlines . map concat $
      [ [" ", " ", s, e, n, d]
      , ["+", " ", m, o, r, e]
      , take 6 (repeat "-")
      , [" ",   m, o, n, e, y]
      ] 

printAnswer :: IO ()
printAnswer = return solve >>= mapM_ (putStrLn . displayAnswer)

sum2Mod10 [s, x, y] = s == (x + y) `mod` 10
sum2Div10 [s, x, y] = s == (x + y) `div` 10

sum3Mod10 [s, x, y, z] = s == (x + y + z) `mod` 10
sum3Div10 [s, x, y, z] = s == (x + y + z) `div` 10

letters = [ 'D', 'E', 'M', 'N', 'O', 'R', 'S', 'Y' ]
carry_names = [ "carry_d_e", "carry_n_r", "carry_e_o", "carry_s_m" ]

solve :: [Answer]
solve = runFD $ do
    digits@[d, e, m, n, o, r, s, y]  <- newVars (length letters) [0..9]
    carries@[c_de, c_nr, c_eo, c_sm] <- newVars 4 [0,1]

    allDifferent digits

    relatedBy3 sum2Mod10 [y, d, e]
    relatedBy3 sum2Div10 [c_de, d, e]

    relatedBy4 sum3Mod10 [e, n, r, c_de]
    relatedBy4 sum3Div10 [c_nr, n, r, c_de]

    relatedBy4 sum3Mod10 [n, e, o, c_nr]
    relatedBy4 sum3Div10 [c_eo, e, o, c_nr]

    relatedBy4 sum3Mod10 [o, s, m, c_eo]
    relatedBy4 sum3Div10 [c_sm, s, m, c_eo]

    m `same` c_sm

    m `avoids` [0]
    s `avoids` [0]

    labelling $ digits ++ carries

displayDomains :: [[Int]] -> String
displayDomains = unlines . map format . zip names
  where
    names = (map (:[]) letters) ++ carry_names
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
    digits@[d, e, m, n, o, r, s, y]  <- newVars (length letters) [0..9]
    carries@[c_de, c_nr, c_eo, c_sm] <- newVars 4 [0,1]

    allDifferent digits

    relatedBy3 sum2Mod10 [y, d, e]
    relatedBy3 sum2Div10 [c_de, d, e]

    relatedBy4 sum3Mod10 [e, n, r, c_de]
    relatedBy4 sum3Div10 [c_nr, n, r, c_de]

    relatedBy4 sum3Mod10 [n, e, o, c_nr]
    relatedBy4 sum3Div10 [c_eo, e, o, c_nr]

    relatedBy4 sum3Mod10 [o, s, m, c_eo]
    relatedBy4 sum3Div10 [c_sm, s, m, c_eo]

    m `same` c_sm

    m `avoids` [0]
    s `avoids` [0]

    -- follow depth-first search

    -- d `hasValue` 1

    getDomains $ digits ++ carries


