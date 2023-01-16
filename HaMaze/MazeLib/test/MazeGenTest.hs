module MazeGenTest (testGen) where

import MazeLib
import System.IO (print)

randoms :: [Int]
randoms = [1..100]

testGen :: IO ()
testGen = print $ mazeGen (5, 5) (1,1) randoms
