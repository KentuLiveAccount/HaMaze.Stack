module MazeGen (mazeGen) where

import Data.Set as Set (Set, member, empty, insert, elems, fromList)
import Data.Maybe (listToMaybe)
import MazeTypes (Bounds)

type Placements = Set.Set (Int, Int)

emptyPl :: Placements
emptyPl = Set.empty

lookupPl :: Placements -> (Int, Int) -> Bool
lookupPl pl pt = Set.member pt pl

includePl :: Placements -> (Int, Int) -> Placements
includePl pl pt = Set.insert pt pl

moves :: [(Int, Int)]
moves = [(-1, 0), (0, -1), (1, 0), (0, 1)]

diagonals :: (Int, Int) -> [(Int, Int)]
diagonals (x, y) = [(x - 1, y - 1), (x + 1, y - 1), (x - 1, y + 1), (x + 1, y + 1)]

surroundings :: (Int, Int) -> [(Int, Int)]
surroundings (x, y) = [(x - 1, y - 1), (x - 1, y), (x - 1, y + 1), (x, y -1), (x, y + 1), (x + 1, y -1), (x + 1, y), (x + 1, y + 1)]

minus :: (Int, Int) -> (Int, Int) -> (Int, Int)
minus (x1, y1) (x2, y2) = (x1 - x2, y1 - y2)

allSame :: (Eq a) => [a] -> Bool
allSame [] = False
allSame [_] = False
allSame (a:as) = all (a==) as

validNeighbors :: [(Int, Int)] -> Bool
validNeighbors [] = False
validNeighbors [_] = True
validNeighbors ns =  allSame (map fst ns) || allSame (map snd ns)            

validCandidate :: Bounds -> Placements -> (Int, Int) -> Bool
validCandidate _ _ (0, _) = False
validCandidate _ _ (_, 0) = False
validCandidate (bx, by) places (x, y)
    | bx == (x + 1) = False
    | by == (y + 1) = False
    | otherwise = validNeighbors $ map (\p -> p `minus`  (x, y)) $ filter (lookupPl places) (surroundings (x, y))

pointAdd :: (Int, Int) -> (Int, Int) -> (Int, Int)
pointAdd (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

takeAtMaybe :: Int -> [a] -> Maybe a
takeAtMaybe _ [] = Nothing
takeAtMaybe _ [a] = Just a
takeAtMaybe i as = Just $ as!!(i `div` (6 `div` (length as)))

nextMove :: Bounds -> Placements -> (Int, Int) -> Int -> Maybe (Int, Int)
nextMove bounds pl cur rnd = takeAtMaybe rnd $ filter (validCandidate bounds pl) $ filter (not . lookupPl pl) $ map (pointAdd cur) moves

data BuildState = BS {bds :: Bounds, path :: [(Int, Int)], spread :: Placements, maxDepth :: Int, goal :: (Int, Int)}

buildMaze :: BuildState -> Int -> [Int] -> BuildState
buildMaze bs@(BS _ [] _ _ _) _ _ = bs
buildMaze (BS bds (p:path) spread mD g) i (r:nds) = 
    case (nextMove bds spread p r) of
        Just next -> buildMaze (BS bds (next:p:path) (includePl spread next) (max mD i) (if mD < i then next else g) ) (i + 1) nds
        Nothing -> buildMaze (BS bds path spread mD g) (i - 1) nds

mazeGen :: (Int, Int) -> (Int, Int) -> [Int] -> ([(Int, Int)], (Int, Int))
mazeGen bounds start rnd = (Set.elems sp, g)
    where
        (BS _ _ sp _ g) =  buildMaze (BS bounds [start] (includePl emptyPl start)  0 start) 0 rnd

