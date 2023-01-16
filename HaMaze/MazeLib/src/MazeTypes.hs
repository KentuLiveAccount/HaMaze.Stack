module MazeTypes (Bounds, Point, PointDelta, InputSpace (..)) where

type Bounds = (Int, Int)

type Point = (Int, Int)
type PointDelta = (Int, Int)
data InputSpace = InputSpace {bounds::Bounds, obstacles::[Point], moves::[PointDelta]} deriving (Show, Eq)
