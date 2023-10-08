module SearchPath (solvePath) where

import Data.Array.IArray as IArray
import Data.Set as Set (Set, fromList, elems)
import Data.Ord
import Data.List (minimumBy)
import MazeTypes


type SearchSpace = (Bounds, IArray.Array Int Int, [PointDelta])

searchSpaceFromInputSpace :: InputSpace -> SearchSpace
searchSpaceFromInputSpace is@(InputSpace bds obs mvs _ _) = (bds, accumArray (+) 0 (0, area bds) $ map (\x -> (coordToIdx bds x, -1)) obs, mvs)

area :: Bounds -> Int
area (dx, dy) = dx * dy

inBounds :: Bounds -> (Int, Int) -> Bool
inBounds (dx, dy) (x, y) = 0 <= x && x < dx && 0 <= y && y < dy

coordToIdx :: Bounds -> (Int, Int) -> Int
coordToIdx (dx, dy) (x, y) = y * dx + x

idxToCoord :: Bounds -> Int -> (Int, Int)
idxToCoord (dx, dy) i = (x, y)
    where
        x = i `mod` dx
        y = i `div` dx


neighbors :: SearchSpace -> (Int, Int) -> [(Int, Int)]
neighbors (sBounds, sp, motion) (x, y) = filter (inBounds sBounds) nbs
    where
        nbs = map (\(dx, dy) -> (x + dx, y + dy)) motion

setStart :: SearchSpace -> (Int, Int) -> Maybe SearchSpace
setStart (sBounds, sp, mvs) start
    | sp!(coordToIdx sBounds start) /= 0 = Nothing
    | otherwise = Just $ (sBounds, sp//[(coordToIdx sBounds start, 1)], mvs)

zeroOrLargerThan :: Int -> Int -> Bool
zeroOrLargerThan c v = (v == 0) || (c < v)

propagateCost :: SearchSpace -> [(Int, Int)] -> (Int, Int) -> Int -> Maybe SearchSpace
propagateCost bsp@(sBounds, sp, mvs) poss end v
    | poss == [] = Nothing
    | poss == [end] = Just (sBounds, sp', mvs)
    | otherwise     = propagateCost (sBounds, sp', mvs) posNext' end (v + 1)
    where
        sp' = accum (\_ a -> a) sp $ map (\x -> (coordToIdx sBounds x, v)) smallers
        smallers = filter (\x -> (zeroOrLargerThan v $ sp!(coordToIdx sBounds x))) poss
        nbs = concatMap (neighbors bsp) smallers
        posNext = Set.elems $ Set.fromList $ nbs
        posNext' = if (elem end posNext) then [end] else posNext

findPath :: (Int, Int) -> (Int, Int) -> [(Int, Int)] -> SearchSpace -> Maybe [(Int, Int)]
findPath  start end path bsp@(sBounds, sp, _) 
    | start == end = Just (end:path)
    | otherwise    = if (ends == []) then Nothing else findPath start end' (end:path) bsp
    where
        ends = filter (\x -> (sp!(coordToIdx sBounds x) == (vend - 1))) $ neighbors bsp end
        end' = if (elem start ends) then start else (head ends)
        vend = sp!(coordToIdx sBounds end)

solvePath :: InputSpace -> Maybe [(Int, Int)]
solvePath is = (findPath orig gl []) =<< propagateCost (searchSpaceFromInputSpace is) [orig] gl 1
    where  
        orig = (isOrigin is)
        gl   = (isGoal is)
    
