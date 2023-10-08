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

origin :: (Int, Int)
origin = (1, 1)

sz :: Int
sz = 40   

sBounds :: Bounds
sBounds = (sz, sz)

randSeq :: [Int]
randSeq =
    let rollsM :: StatefulGen g m => Int -> g -> m [Int]
        rollsM n = replicateM n . uniformRM (0, 5)
        pureGen = mkStdGen 137
    in
        runStateGen_ pureGen (rollsM 13) :: [Int]

sampleMap :: InputSpace
sampleMap = mazeGen sBounds origin (cycle randSeq)

obstacleColor = rgb 0x00 0x88 0x88
pathColor     = rgb 0xff 0x00 0x00
startColor    = rgb 0x00 0xff 0x00
endColor      = rgb 0x00 0x00 0xff

type MyAppState = AppState (Maybe [(Int, Int)])

initialState :: MyAppState
initialState = (emptyState $ solvePath sampleMap) {blocks = obstackleCol, initialSize = (600, 600), transformer = updateAppState}
    where
        obstackleCol = (addColor startColor origin) : (addColor endColor goal) : (map (addColor obstacleColor) ob)
        goal = isGoal sampleMap
        ob   = isObstacles sampleMap

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
    print $ solvePath sampleMap
    startApp initialState
