module Main

import Common.Parser

import Data.List
import Data.SortedSet as S
import System.File.ReadWrite

parseSteps : Parser String (List Char)
parseSteps = concat <$> pLinesOf parseStep

    where
    parseStep : Parser (List Char) (List Char)
    parseStep = do
        dir <- pNext
        _   <- pItem ' '
        num <- pNat
        pure $ replicate num dir

data Pos =
    MkPos Int Int

data Rope =
    MkRope Pos Pos

Eq Pos where
    MkPos a b == MkPos c d = a == c && b == d

Ord Pos where
    MkPos a b > MkPos c d = (a, b) > (c, d)
    MkPos a b < MkPos c d = (a, b) < (c, d)

Show Pos where
    show (MkPos a b) = concat ["(", show a, ", ", show b, ")"]

stepHead : Pos -> Char -> Pos
stepHead (MkPos hx hy) x =
    case x of
        'U' => MkPos  hx   (hy-1)
        'D' => MkPos  hx   (hy+1)
        'L' => MkPos (hx-1) hy
        'R' => MkPos (hx+1) hy
        _   => MkPos  hx    hy

followHead : Pos -> Pos -> Pos
followHead (MkPos hx hy) (MkPos tx ty) =
    let (dx', dy') =
        case (hx - tx, hy - ty) of
            (-2,  0) => (-1,  0)
            ( 2,  0) => ( 1,  0)
            ( 0, -2) => ( 0, -1)
            ( 0,  2) => ( 0,  1)
            ( a,  b) =>
                if abs a + abs b <= 2
                    then (0, 0)
                    else (unit a, unit b)
    in MkPos (tx + dx') (ty + dy')
    where
    unit : Int -> Int
    unit 0 = 0
    unit a = div a (abs a)

stepRope : Char -> Rope -> Rope
stepRope dir (MkRope headPos tailPos) =
    let headPos' = stepHead headPos dir
        tailPos' = followHead headPos' tailPos
    in MkRope headPos' tailPos'

part1 : List Char -> Nat
part1 path =
    let headStart = MkPos 0 0
        tailStart = MkPos 0 0
    in step (singleton tailStart) (MkRope headStart tailStart) path
    where
    step : SortedSet Pos -> Rope -> List Char -> Nat
    step tailPoses    _      [] = length $ S.toList tailPoses
    step tailPoses rope (x::xs) =
        let rope'@(MkRope a b) = stepRope x rope
        in step (insert b tailPoses) rope' xs

main : IO ()
main =

    case !(readFile "./day09/input") of

        Left l =>
            putStrLn "Could not read input file"

        Right contents =>

            case runParser parseSteps contents of

                Left l =>
                    putStrLn $ "Did not parse input correctly: " ++ l

                Right ("", path) => do
                    printLn $ part1 path

                Right _ =>
                    putStrLn "Did not parse input correctly"
