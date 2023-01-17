{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Main where

import WinLib
import Data.Bits
import qualified Data.Set as Set
import Graphics.Win32

import MazeLib
import System.Random
import System.Random.Stateful
import Control.Monad (replicateM)


x = 1
s = 2
g = 3
mz :: [[Int]]
mz = [
    [x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x],
    [x,s,x,0,0,0,x,0,x,0,0,0,0,0,0,0,x,0,0,0,x],
    [x,0,x,0,x,0,x,0,x,0,x,0,x,x,x,0,x,x,x,0,x],
    [x,0,0,0,x,0,x,0,0,0,x,0,x,0,x,0,0,0,0,0,x],
    [x,x,x,x,x,0,x,x,x,x,x,0,x,0,x,x,x,x,x,0,x],
    [x,0,0,0,x,0,0,0,0,0,0,0,x,0,0,0,0,0,x,0,x],
    [x,0,x,0,x,x,x,x,x,x,x,x,x,0,x,0,x,x,x,0,x],
    [x,0,x,0,x,0,0,0,0,0,0,0,0,0,x,0,x,0,0,0,x],
    [x,0,x,0,x,0,x,x,x,x,x,0,x,0,x,0,x,0,x,x,x],
    [x,0,x,0,x,0,x,0,0,0,x,0,x,0,x,0,0,0,x,0,x],
    [x,0,x,0,0,0,x,0,0,0,x,0,x,0,x,x,x,x,x,0,x],
    [x,0,x,x,x,x,x,x,x,0,x,0,x,0,0,0,x,0,0,0,x],
    [x,0,x,0,0,0,0,0,0,0,x,0,x,0,x,0,x,0,0,0,x],
    [x,0,x,x,x,x,x,0,x,0,x,0,x,x,x,0,x,x,x,0,x],
    [x,0,0,0,0,0,x,0,x,x,x,0,0,0,x,0,0,0,x,0,x],
    [x,x,x,x,x,0,x,0,0,0,0,0,x,x,x,x,x,0,x,0,x],
    [x,0,0,0,x,0,x,x,x,x,x,0,x,0,0,0,0,0,x,0,x],
    [x,0,x,0,x,0,x,0,0,0,x,0,x,0,x,x,x,x,x,0,x],
    [x,0,x,0,0,0,x,0,x,0,x,0,x,0,0,0,0,0,x,0,x],
    [x,0,x,x,x,x,x,0,x,0,x,x,x,x,x,x,x,0,x,0,x],
    [x,0,0,0,0,0,0,0,x,0,0,0,0,0,0,0,0,0,0,g,x],
    [x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x]]

goal :: (Int, Int)

paths :: [(Int, Int)]
--ob = map snd $ filter (\(v, _) -> v == x) $ concat $ zipWith (\y (xs) -> zipWith (\x p -> (p, (x, y))) [0..] xs) [0..] mz
--goal = (19, 20)

sz :: Int
sz = 40   

randSeq :: [Int]
randSeq =
    let rollsM :: StatefulGen g m => Int -> g -> m [Int]
        rollsM n = replicateM n . uniformRM (0, 5)
        pureGen = mkStdGen 137
    in
        runStateGen_ pureGen (rollsM 10) :: [Int]

--(paths, goal)  = mazeGen (sz, sz) (1, 1) (cycle [0..5])
(paths, goal)  = mazeGen (sz, sz) (1, 1) (cycle randSeq)

ob :: [(Int, Int)]
ob = filter (not . (\x -> elem x paths)) [(x, y) | x <- [0..(sz - 1)], y <- [0..(sz - 1)]]

obstacles' :: [(Int, Int)]
--obstacles' = [(0, 5), (5, 1), (1, 4), (2, 2), (2, 3), (4, 4)]
obstacles' = [(0, 1), (1, 1), (2, 1), (3, 1), (4, 1), (5, 3), (4, 3), (3, 3), (2, 3), (1, 3), (1, 4), (3, 5)]

origin :: (Int, Int)
origin = (1, 1)


sBounds :: Bounds
sBounds = (sz, sz) -- iStart + cWidth

moves' :: [(Int, Int)]
moves' = [(-1, 0), (0, -1), (1, 0), (0, 1)] -- no diagonal

sampleMap = InputSpace sBounds ob moves'

obstacleColor = rgb 0x00 0x88 0x88
pathColor = rgb 0xff 0x0 0x0
startColor = rgb 0x00 0xff 0x00
endColor = rgb 0x00 0x00 0xff

obstackleCol = (addColor startColor origin) : (addColor endColor goal) : (map (addColor obstacleColor) ob)

type MyAppState = AppState (Maybe [(Int, Int)])

initialState :: MyAppState
initialState = (emptyState (solvePath sampleMap origin goal)) {blocks = obstackleCol, initialSize = (600, 600), transformer = updateAppState}

nxt :: [(Int, Int, COLORREF)] -> Maybe [(Int, Int)] -> ([(Int, Int, COLORREF)], Maybe [(Int, Int)])
nxt bls Nothing = (bls, Nothing)
nxt bls (Just [a]) = ((addColor pathColor a):bls, Nothing)
nxt bls (Just (a:as)) = ((addColor pathColor a):bls, Just as)

updateAppState :: MyAppState -> (MyAppState, [RECT])
updateAppState as = let
        paths = appdata as
        (bls', pths') = nxt (blocks as) paths
    in
        (as {blocks = bls', appdata = pths'}, [windowRect as])
        --(as, [])
         --(as {blocks = nextBlocks (cx, cy) (blocks as)}, [windowRect as])

main = do
    print $ solvePath sampleMap origin goal
    startApp initialState
