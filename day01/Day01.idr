module Main

import Data.List1
import Data.String
import System.File.ReadWrite

asGroups : String -> Maybe (List1 (List1 Int))
asGroups = fromList
         . map (map cast)
         . groupWith (== "")
         . lines

part1 : List1 (List1 Int) -> Int
part1 = foldl1 max . map sum

part2 : List1 (List1 Int) -> Int
part2 = sum . take 3 . reverse . sort . forget . map sum

main : IO ()
main =

    case !(readFile "./day01/input") of

        Left l =>
            putStrLn "Could not read input file"

        Right contents =>
            case asGroups contents of
                Nothing =>
                    putStrLn "Could not find any gorups in input"
                Just groups => do
                    printLn $ part1 groups
                    printLn $ part2 groups
