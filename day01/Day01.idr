module Main

import Data.List
import Data.List1
import Data.String
import System.File.ReadWrite

partial
fromRight : Either l r -> r
fromRight (Left l)  = idris_crash "Unexpected left"
fromRight (Right r) = r

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

maximum : List Int -> Maybe Int
maximum = go Nothing
    where
    go : Maybe Int -> List Int -> Maybe Int
    go        acc      [] = acc
    go    Nothing (x::xs) = go (Just x) xs
    go j@(Just y) (x::xs) =
    if y > x
        then go j xs
        else go (Just x) xs

partial
part1 : List (List Int) -> Int
part1 = fromJust . maximum . map sum

partial
part2 : List (List Int) -> Int
part2 = sum . take 3 . reverse . sort . map sum

partial
main : IO ()
main = do
    contents <- fromRight <$> readFile "./input"
    let groups = asGroups contents
    printLn $ part1 groups
    printLn $ part2 groups
