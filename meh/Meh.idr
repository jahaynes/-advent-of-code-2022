module Main

import Common.AStar
import Common.Heap as H
import Debug.Trace

data Cost =
    MkCost Int

Eq Cost where
    MkCost a == MkCost b = a == b

Ord Cost where
    MkCost a < MkCost b = a < b

Semigroup Cost where
    MkCost a <+> MkCost b = MkCost $ a + b

Monoid Cost where
    neutral = MkCost 0

Show Cost where
    show (MkCost a) = show a

start : Start (Int, Int)
start = MkStart (0, 0)

goal : Goal (Int, Int)
goal = MkGoal (5, 5)

heuristic : (Int, Int) -> Goal (Int, Int) -> Cost
heuristic (x, y) (MkGoal (x', y')) = MkCost $ ( max x x' - min x x'
                                              + max y y' - min y y' )

expand : (Int, Int) -> Cost -> List ((Int, Int), Cost)
expand (x, y) (MkCost cost) =
    let cost' = MkCost $ cost + 1
        expansions = [ ((x+1, y  ), cost')
                     , ((x  , y+1), cost')
                     , ((x-1, y  ), cost')
                     , ((x  , y-1), cost') ]
    in expansions

main : IO ()
main = do

    let solver = MkSolver start
                          goal
                          heuristic
                          expand

    findPath solver
