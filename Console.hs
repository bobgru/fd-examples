module Console (
    Pos,
    beep, cls, goto, writeAt, seqn, rmdups, wait
) where

beep :: IO ()
beep =  putStr "\BEL"

cls :: IO ()
cls = putStr "\ESC[2J"

type Pos = (Int, Int)

goto :: Pos -> IO ()
goto (x,y) = putStr ("\ESC[" ++ show(y) ++ ";" ++ show(x) ++ "H")

writeAt :: Pos -> String -> IO ()
writeAt p xs = do goto p
                  putStr xs

seqn :: [IO a] -> IO ()
seqn [] = return ()
seqn (a:as) = do a
                 seqn as

rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x:xs) = x : rmdups (filter (/= x) xs)

wait :: Int -> IO ()
wait n = seqn [ return () | _ <- [1..n]]
