module Common.AStar

import Common.Heap as H

import Data.SortedSet as S

public export
data Start coord =
    MkStart coord

Eq coord => Eq (Start coord) where
    MkStart c1 == MkStart c2 =
        c1 == c2

public export
data Goal coord =
    MkGoal coord

Eq coord => Eq (Goal coord) where
    MkGoal c1 == MkGoal c2 =
        c1 == c2

public export
record Step coord cost where
    constructor MkStep
    getCoord : coord
    getCost  : cost

(Eq coord, Eq cost) => Eq (Step coord cost) where
    MkStep coord1 cost1 == MkStep coord2 cost2 =
        (cost1, coord1) == (cost2, coord2)

(Ord coord, Ord cost) => Ord (Step coord cost) where
    MkStep coord1 cost1 < MkStep coord2 cost2 =
        (cost1, coord1) < (cost2, coord2)

(Show coord, Show cost) => Show (Step coord cost) where
    show (MkStep coord cost) = "(" ++ show coord ++ ", " ++ show cost ++ ")"

public export
record Solver coord cost where
    constructor MkSolver
    getStart  : Start coord
    getGoal   : Goal coord
    heuristic : coord -> Goal coord -> cost
    expand    : coord -> cost -> List (coord, cost)

export
findPath : ( Show coord
           , Show cost
           , Monoid cost
           , Ord cost
           , Ord coord ) => Solver coord cost
                         -> IO ()
findPath solver =
    let MkStart start = getStart solver
    in go (S.singleton start) H.empty (MkStep start neutral)
    where
    go : SortedSet coord
      -> Heap (cost, Step coord cost)
      -> Step coord cost
      -> IO ()
    go done fringe step =

        if MkGoal step.getCoord == solver.getGoal

            then
                putStrLn "Done"

            else

                let next = filter (\(coord, _) => not $ S.contains coord done)
                         $ solver.expand step.getCoord step.getCost

                    done' = foldr S.insert done 
                          $ map fst next

                    fringe' = foldl H.insert fringe
                            $ map makeHeuristicStep next

                in case H.deleteMin fringe' of

                    Nothing =>
                        printLn "Empty fringe"

                    Just ((_, f), ringe) => do
                        printLn f
                        go done' ringe f
        where
        makeHeuristicStep : (coord, cost) -> (cost, Step coord cost)
        makeHeuristicStep (coord, cost) = 
            let totalCost = cost <+> solver.heuristic coord solver.getGoal
            in (totalCost, MkStep coord cost)
