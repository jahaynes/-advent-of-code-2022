module Main

import Data.SortedMap
import Data.String
import System.File.ReadWrite

data Cell =
    MkCell Char Bool

Show Cell where
    show (MkCell c vis) =
        let v = if vis then '_' else '*'
        in pack (c::v::[])

fromChar : Char -> Cell
fromChar c = MkCell c False

part1 : List (List Cell) -> Nat
part1 = totalVis . sweepAndRotate 4
    where
    totalVis : List (List Cell) -> Nat
    totalVis = sum . concatMap (map (\(MkCell _ v) => if v then 1 else 0))

    sweepAndRotate : Nat -> List (List Cell) -> List (List Cell)
    sweepAndRotate     Z acc = acc
    sweepAndRotate (S n) acc = sweepAndRotate n (rotate $ map sweepRow acc)
        where
        rotate : List (List a) -> List (List a)
        rotate = reverse . transpose

        sweepRow : List Cell -> List Cell
        sweepRow = go Nothing []
        where
            go : Maybe Char -> List Cell -> List Cell -> List Cell
            go    mPrev acc               [] = reverse acc
            go  Nothing acc (MkCell c _::cs) = go (Just c) (MkCell c True::acc) cs
            go (Just p) acc (MkCell c v::cs) = go (Just (max p c)) (MkCell c (v || p < c)::acc) cs

withCoords : List (List a) -> List ((Nat, Nat), a)
withCoords grid = do
    (r, row)  <- zip [0..length grid] grid
    (c, cell) <- zip [0..length row] row
    pure ((r, c), cell)

scenicScore : SortedMap (Nat, Nat) Cell -> Nat -> Nat -> Maybe Nat
scenicScore grid sr sc =

    case lookup (sr, sc) grid of
        Nothing           => Nothing
        Just (MkCell h _) =>
            Just $ product [ step  leftward 0 h sr sc
                           , step rightward 0 h sr sc
                           , step    upward 0 h sr sc
                           , step  downward 0 h sr sc ]

    where
    leftward : Nat -> Nat -> Maybe (Nat, Nat)
    leftward r     Z = Nothing
    leftward r (S c) = Just (r, c)

    rightward : Nat -> Nat -> Maybe (Nat, Nat)
    rightward r c = Just (r, S c)

    upward : Nat -> Nat -> Maybe (Nat, Nat)
    upward     Z c = Nothing
    upward (S r) c = Just (r, c)

    downward : Nat -> Nat -> Maybe (Nat, Nat)
    downward r c = Just (S r, c)

    step : (Nat -> Nat -> Maybe (Nat, Nat))
          -> Nat
          -> Char
          -> Nat
          -> Nat
          -> Nat
    step stepFun acc h r c =
        case stepFun r c of
            Nothing       => acc
            Just (r', c') =>
                case lookup (r', c') grid of
                    Nothing            => acc
                    Just (MkCell h' _) =>
                        let acc' = acc + 1 in
                        if h' < h
                            then step stepFun acc' h r' c'
                            else acc'

allScores : List (List Cell) -> SortedMap (Nat, Nat) Cell -> List (Nat, Nat, Maybe Nat)
allScores grid grid' = do
    (r, row)  <- zip [0..length grid] grid
    (c, cell) <- zip [0..length row] row
    pure (r, c, scenicScore grid' r c)

part2 : List (List Cell) -> Maybe Nat
part2 grid =
    let grid' = fromList $ withCoords grid
        allScos = allScores grid grid'
    in foldl max Nothing . map (\(_, _, mv) => mv) $ allScos

main : IO ()
main =

    case !(readFile "./day08/input") of

        Left l =>
            putStrLn "Could not read input file"

        Right contents => do

            let grid = map (map fromChar . unpack)
                     . lines
                     $ contents

            printLn $ part1 grid
            printLn $ part2 grid
