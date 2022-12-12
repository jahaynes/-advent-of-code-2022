module Main

import Data.SortedMap as M
import Data.SortedSet as S
import Data.String
import System.File.ReadWrite

record Pos where
    constructor MkPos
    row : Nat
    col : Nat

Show Pos where
    show (MkPos row col) =
        concat [ "("
               , show row
               , ", "
               , show col
               , ")" ]

record Agent where
    constructor MkAgent
    pos  : Pos
    elev : Char

data Start =
    MkStart Pos

Show Start where
    show (MkStart pos) = show pos

data End =
    MkEnd Pos

Show End where
    show (MkEnd pos) = show pos

bar : Nat -> List Char -> List Char
bar a b = b

foo : Nat -> List Char -> List (Nat, Char)
foo rowN row = zip [0..length row] row

partial
findF : (Char -> Bool) -> List (List Char) -> Maybe Pos
findF f grid = do

    let grid' = zipWith foo [0..length grid] grid

    idris_crash $ unlines $ map show grid' -- "meh"

partial
main : IO ()
main =

    case !(readFile "./day12/sample_input") of

        Left l =>
            putStrLn "Could not read input file"

        Right contents => do

            let grid = map unpack $ lines contents

            let x = findF (const True) grid

            printLn x
