module MazeTypes (Bounds, Point, PointDelta, InputSpace (..)) where

type Bounds = (Int, Int)

type Point = (Int, Int)
type PointDelta = (Int, Int)
data InputSpace = InputSpace {isBounds::Bounds, isObstacles::[Point], isMoves::[PointDelta], isOrigin::Point, isGoal::Point} deriving (Show, Eq)
