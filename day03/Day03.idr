module Main

import Data.List1 as L1
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

groupsOf : Nat -> List a -> List (List1 a)
groupsOf n = go []
    where
    go : List (List1 a) -> List a -> List (List1 a)
    go acc xs =
        let (some, rest) = splitAt n xs
        in case L1.fromList some of
            Just some1 => go (some1 :: acc) rest
            Nothing    => reverse acc

part2 : String -> Int
part2 = sum
      . mapMaybe (`lookup` prios)
      . concatMap S.toList
      . map (foldl1 intersection)
      . groupsOf 3
      . map (S.fromList . unpack)
      . lines

main : IO ()
main =

    case !(readFile "./day03/input") of

        Left l =>
            putStrLn "Could not read input file"

        Right contents => do
            printLn $ part1 contents
            printLn $ part2 contents
