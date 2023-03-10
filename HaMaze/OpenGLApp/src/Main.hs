module Main (main) where

import Graphics.UI.GLUT
import Data.IORef
import Data.Bits
import qualified Data.Set as Set

import MazeLib
import System.Random
import System.Random.Stateful
import Control.Monad (replicateM)

goal :: (Int, Int)

paths :: [(Int, Int)]

origin :: (Int, Int)
origin = (1, 1)

sz :: Int
sz = 40   

sBounds :: Bounds
sBounds = (sz, sz) -- iStart + cWidth

moves' :: [(Int, Int)]
moves' = [(-1, 0), (0, -1), (1, 0), (0, 1)] -- no diagonal

randSeq :: [Int]
randSeq =
    let rollsM :: StatefulGen g m => Int -> g -> m [Int]
        rollsM n = replicateM n . uniformRM (0, 5)
        pureGen = mkStdGen 191
    in
        runStateGen_ pureGen (rollsM 10) :: [Int]

--(paths, goal)  = mazeGen (sz, sz) (1, 1) (cycle [0..5])
(paths, goal)  = mazeGen (sz, sz) origin (cycle (randSeq))

ob :: [(Int, Int)]
ob = filter (not . (\x -> elem x paths)) [(x, y) | x <- [0..(sz - 1)], y <- [0..(sz - 1)]]

solutionPath :: Maybe [(Int, Int)]
solutionPath = solvePath (InputSpace sBounds ob moves') origin goal

obstacleColor :: (GLfloat,GLfloat,GLfloat)
obstacleColor = (0, 0.5, 0.5)
pathColor :: (GLfloat,GLfloat,GLfloat)
pathColor = (1.0, 0, 0)

startColor :: (GLfloat,GLfloat,GLfloat)
startColor = (0, 1.0, 0)

endColor :: (GLfloat,GLfloat,GLfloat)
endColor = (0, 0, 1.0)

divisor :: GLfloat
divisor = fromIntegral sz

dxy :: GLfloat
dxy = 0.9 * (2.0 / divisor) - (2.0 / (divisor * 10.0))



myPoints :: (GLfloat, GLfloat, GLfloat) -> [(Int, Int)] -> [(GLfloat,GLfloat,GLfloat, (GLfloat, GLfloat, GLfloat))]
myPoints c pts =  map (\(x, y) -> (scale x, scale y * (-1.0), 0.0, c)) pts
  where
    scale x = 0.9 * ((((fromIntegral x) * 2.0) / divisor) - 1.0)

stationaries :: [(GLfloat,GLfloat,GLfloat, (GLfloat, GLfloat, GLfloat))]
stationaries = (myPoints startColor [origin]) ++ (myPoints endColor [goal]) ++ (myPoints obstacleColor ob)



toRect :: (GLfloat,GLfloat,GLfloat) -> [(GLfloat,GLfloat,GLfloat)]
toRect (x, y, z) = [(x, y, z), (x + dxy, y, z), (x + dxy, y + dxy, z), (x, y + dxy, z)]

rectVertex :: (GLfloat,GLfloat,GLfloat, (GLfloat,GLfloat,GLfloat)) -> IO ()
rectVertex (x, y, z, c) = mapM_ (\(x, y, z) ->
  (setColor c) >> vertex (Vertex3 x y z)) $ toRect (x, y, z)

data AppState = AS {step :: Int, interval :: Timeout}

initialAppState :: AppState
initialAppState = (AS 0 16)

nextAppState :: AppState -> AppState
nextAppState (AS st to) = (AS st' to)
  where
    st' = (st + 1) `mod` (lengthOr 5 solutionPath)
    lengthOr :: Int -> Maybe [a] -> Int
    lengthOr n Nothing = n
    lengthOr _ (Just as) = length as

main :: IO ()
main = do
  print $ myPoints (1, 0, 0) ob
  (_progName, _args) <- getArgsAndInitialize
  stateRef <- newIORef initialAppState
  initialDisplayMode $= [DoubleBuffered]
  _window <- createWindow "Hello World"
  displayCallback $= (display stateRef)
  addTimerCallback 16 (timerProc stateRef)
  mainLoop

setColor :: (GLfloat, GLfloat, GLfloat) -> IO ()
setColor (r, g, b) = color3f r g b
  where
    color3f :: GLfloat -> GLfloat -> GLfloat -> IO ()
    color3f r g b = color $ Color3 r g b

display :: IORef AppState -> DisplayCallback
display ior = do 
  (AS dl _) <- readIORef ior
  clear [ColorBuffer] -- ColorBuffer :: ClearBuffer
  preservingMatrix ( do
    scale 0.8 0.8 (0.8::GLfloat)
    -- renderPrimitive :: PrimitiveMode -> IO a -> IO a
    mapM_ (\pts -> renderPrimitive Quads $ mapM_ rectVertex (myPoints pathColor (take dl pts))) solutionPath
    renderPrimitive Quads $ mapM_ rectVertex stationaries)
  swapBuffers

timerProc :: IORef AppState -> IO ()
timerProc ior = do
    (AS _ timeout) <- readIORef ior
    addTimerCallback timeout $ timerProc ior
    modifyIORef ior nextAppState
    postRedisplay Nothing
