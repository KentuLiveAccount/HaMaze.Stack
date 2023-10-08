module SearchPathTest where

import MazeLib

unJust :: Maybe a -> a
unJust (Just a) = a
{-
printBoard :: SearchSpace -> IO ()
printBoard (sBounds, sp, _) = mapM_ (putStrLn . show) lns
    where
        lns = map (\y -> map (\x -> sp!(coordToIdx sBounds (x, y))) [0..5]) [0..5]
-}

obstacles' :: [(Int, Int)]
--obstacles' = [(0, 5), (5, 1), (1, 4), (2, 2), (2, 3), (4, 4)]
obstacles' = [(0, 1), (1, 1), (2, 1), (3, 1), (4, 1), (5, 3), (4, 3), (3, 3), (2, 3), (1, 3), (1, 4), (3, 5)]

origin :: (Int, Int)
origin = (0, 0)

goal :: (Int, Int)
goal = (5, 5)

sBounds :: Bounds
sBounds = (6, 6) -- iStart + cWidth

moves' :: [(Int, Int)]
moves' = [(-1, 0), (0, -1), (1, 0), (0, 1)] -- no diagonal

sampleMap = InputSpace sBounds obstacles' moves' origin goal

testSearchPath = print $ solvePath sampleMap 