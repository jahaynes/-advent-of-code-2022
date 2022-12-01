module Main

import Data.List
import Data.List1
import Data.String
import System.File.ReadWrite

partial
fromJust : Maybe a -> a
fromJust Nothing  = idris_crash "Unexpected Nothing"
fromJust (Just x) = x

asGroups : String -> List (List Int)
asGroups = map (map cast)
         . filter (/= [""])
         . map forget
         . groupWith (== "")
         . lines

total
maximum : List Int -> Maybe Int
maximum      [] = Nothing
maximum (x::xs) = Just $ go x xs
    where
    go : Int -> List Int -> Int
    go acc      [] = acc
    go acc (x::xs) = go (max acc x) xs

partial
part1 : List (List Int) -> Int
part1 = fromJust . maximum . map sum

part2 : List (List Int) -> Int
part2 = sum . take 3 . reverse . sort . map sum

partial
main : IO ()
main =

    case !(readFile "./input") of

        Left l =>
            putStrLn "Could not read input file"

        Right contents => do
            let groups = asGroups contents
            printLn $ part1 groups
            printLn $ part2 groups
