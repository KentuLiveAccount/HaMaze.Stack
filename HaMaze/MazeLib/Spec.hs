{-
main = do
    print $ map (pointAdd (1, 3)) moves
    print $ diagonals (1, 3)
    print $ nextMove (5, 5) (Set.fromList [(1,1),(1,2)]) (1,3) 0
    print $ mazeGen (5, 5) (1,1) (repeat 1)
-}