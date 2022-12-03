module Main

import Data.SortedMap
import Data.SortedSet as S
import Data.String
import System.File.ReadWrite

prios : SortedMap Char Int
prios = fromList $ zip ['a'..'z'] [ 1..26]
                ++ zip ['A'..'Z'] [27..52]

part1 : String -> Int
part1 = sum . map commonItemPrios . lines
    where
    commonItemPrios : String -> Int
    commonItemPrios line =
        let chars   = unpack line
            halfLen = cast (cast (length line) `div` 2)
            first   = fromList $ take halfLen chars
            second  = fromList $ drop halfLen chars
            common  = S.toList $ intersection first second
        in sum $ mapMaybe (`lookup` prios) common

main : IO ()
main =

    case !(readFile "./day03/input") of

        Left l =>
            putStrLn "Could not read input file"

        Right contents =>
            printLn $ part1 contents
