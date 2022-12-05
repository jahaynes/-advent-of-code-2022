module Main

import Common.Parser

import System.File.ReadWrite

parseAssignment : Parser (List Char) (Nat, Nat)
parseAssignment = do
    from <- pNat
    _    <- pItem '-'
    to   <- pNat
    pure (from, to)

parseAssignmentPair : Parser (List Char) ((Nat, Nat), (Nat, Nat))
parseAssignmentPair = do
    first  <- parseAssignment
    _      <- pItem ','
    second <- parseAssignment
    pure (first, second)

part1 : List ((Nat, Nat), (Nat, Nat)) -> Nat
part1 = length
      . filter id
      . map fullyContained
    where
    fullyContained : ((Nat, Nat), (Nat, Nat)) -> Bool
    fullyContained (a, b) = aFullyContainsB a b
                         || aFullyContainsB b a
        where
        aFullyContainsB : (Nat, Nat) -> (Nat, Nat) -> Bool
        aFullyContainsB (a, b) (c, d) = a <= c && b >= d

part2 : List ((Nat, Nat), (Nat, Nat)) -> Nat
part2 = length
      . filter id
      . map overlap
    where
    overlap : ((Nat, Nat), (Nat, Nat)) -> Bool
    overlap (a, b) = not $ disjoint a b
        where
        disjoint : (Nat, Nat) -> (Nat, Nat) -> Bool
        disjoint (a, b) (c, d) = b < c || a > d

main : IO ()
main =

    case !(readFile "./day04/input") of

        Left l =>
            putStrLn "Could not read input file"

        Right contents => do

            case runParser (pLinesOf parseAssignmentPair) contents of

                Right ("", assignmentPairs) => do
                    printLn $ part1 assignmentPairs
                    printLn $ part2 assignmentPairs

                Right (s, _) =>
                    putStrLn $ "Leftover: " ++ s

                Left l =>
                    putStrLn l
