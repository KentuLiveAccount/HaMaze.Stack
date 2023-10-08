module MazeGen (mazeGen) where

import Data.Set as Set (Set, member, empty, insert, elems, fromList)
import MazeTypes (Bounds, InputSpace(..))

{-
    Done: mazeGen should generate InputSpace
-}

type Placements = Set.Set (Int, Int)

emptyPl :: Placements
emptyPl = Set.empty

lookupPl :: Placements -> (Int, Int) -> Bool
lookupPl pl pt = Set.member pt pl

includePl :: Placements -> (Int, Int) -> Placements
includePl pl pt = Set.insert pt pl

moves :: [(Int, Int)]
moves = [(-1, 0), (0, -1), (1, 0), (0, 1)]

diagonals :: [(Int, Int)]
diagonals = [(-1, -1), (1, -1), (-1, 1), (1, 1)]

surroundings = moves ++ diagonals

onFstSnd :: (a -> b -> c) -> (a, a) -> (b, b) -> (c, c)
onFstSnd f (a1, a2) (b1, b2) = (f a1 b1, f a2 b2) 

plus  :: (Int, Int) -> (Int, Int) -> (Int, Int)
plus = onFstSnd (+)

minus :: (Int, Int) -> (Int, Int) -> (Int, Int)
minus = onFstSnd (-)

allSame :: (Eq a) => [a] -> Bool
allSame (a:as) = all (a==) as
allSame _ = False

validNeighbors :: [(Int, Int)] -> Bool
validNeighbors [] = False
validNeighbors [_] = True
validNeighbors ns =  allSame (map fst ns) || allSame (map snd ns)            

-- Bounds :: shows the size of maze space
-- Placements :: map of the maze with obstacles
-- point of inquiry
-- return True if it is a valid point to move to
-- valid candidate point has neighbors only on one column or row (defined by validateNeighbors)
validCandidate :: Bounds -> Placements -> (Int, Int) -> Bool
validCandidate _ _ (0, _) = False -- outer rim of the space is supposed to be all walls
validCandidate _ _ (_, 0) = False -- outer rim of the space is supposed to be all walls
validCandidate (bx, by) places xy@(x, y)
    | bx == (x + 1) = False  -- outer rim of the space is supposed to be all walls
    | by == (y + 1) = False  -- outer rim of the space is supposed to be all walls
    | otherwise = validNeighbors $ map (`minus`  xy) $ filter (lookupPl places) (map (plus xy) surroundings)

takeAtMaybe :: Int -> [a] -> Maybe a
takeAtMaybe _ [] = Nothing
takeAtMaybe _ [a] = Just a
takeAtMaybe i as = Just $ as!!(i `div` (6 `div` (length as)))

nextMove :: Bounds -> Placements -> (Int, Int) -> Int -> Maybe (Int, Int)
nextMove bounds pl cur rnd = takeAtMaybe rnd $ filter (validCandidate bounds pl) unoccupiedNeighbors
    where
        unoccupiedNeighbors = filter (not . lookupPl pl) $ map (plus cur) moves

data BuildState = BS {bds :: Bounds, path :: [(Int, Int)], spread :: Placements, maxDepth :: Int, goal :: (Int, Int)}

buildMaze :: BuildState -> Int -> [Int] -> BuildState
buildMaze bs@(BS _ [] _ _ _) _ _ = bs
buildMaze (BS bds path@(p:ath) spread mD g) i (r:nds) = 
    case (nextMove bds spread p r) of
        Just next -> buildMaze (BS bds (next:path) (includePl spread next) (max mD i) (if mD < i then next else g) ) (i + 1) nds
        Nothing   -> buildMaze (BS bds ath spread mD g) (i - 1) nds

_mazeGen :: (Int, Int) -> (Int, Int) -> [Int] -> ([(Int, Int)], (Int, Int))
_mazeGen bounds start rnd = (Set.elems sp, g)
    where
        (BS _ _ sp _ g) =  buildMaze (BS bounds [start] (includePl emptyPl start)  0 start) 0 rnd

mazeGen :: (Int, Int) -> (Int, Int) -> [Int] -> InputSpace
mazeGen bounds@(bx, by) start rnd = (InputSpace bounds ob moves start gl)
    where
        (paths, gl) = _mazeGen bounds start (cycle (rnd))
        ob :: [(Int, Int)]
        ob = filter (not . (\x -> elem x paths)) [(x, y) | x <- [0..(bx - 1)], y <- [0..(by - 1)]]
