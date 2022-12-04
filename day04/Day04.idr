module Main

import Data.String
import System.File.ReadWrite

data Parser s a =
    MkParser (s -> Either String (s, a))

runParser : Parser s a -> s -> Either String (s, a)
runParser (MkParser run) s = run s

Functor (Parser s) where

    map f (MkParser run) = MkParser $ \s =>
        case run s of
            Right (s', a) => Right (s', f a)
            Left l        => Left l

Applicative (Parser s) where

    pure x = MkParser $ \s => Right (s, x)

    MkParser runf <*> MkParser runx = MkParser $ \s =>
        case runf s of
            Left l => Left l
            Right (s', f) =>
                case runx s' of
                    Left l => Left l
                    Right (s'', x) => Right (s'', f x)

Monad (Parser s) where

    MkParser runa >>= f = MkParser $ \s =>
        case runa s of
            Left l => Left l
            Right (s', a) =>
                let MkParser r = f a
                in r s'

pTake : Nat -> Parser (List Char) (List Char)
pTake n = MkParser $ \s =>
    let (some, rest) = splitAt n s
    in if length some < n
        then Left "Not enough input"
        else Right (rest, some)

pTakeWhile : (s -> Bool) -> Parser (List s) (List s)
pTakeWhile p = MkParser $ \s =>
    let some = takeWhile p s
    in Right (drop (length some) s, some)

pLinesOf : Parser (List Char) a
        -> Parser String (List a)
pLinesOf (MkParser run) = MkParser $ \s => go [] (lines s)
    where
    go : List a
      -> List String
      -> Either String (String, List a)
    go acc [] = Right ("", reverse acc)
    go acc (x::xs) =
        case run (unpack x) of
            Left l        => Left $ "Within pLinesOf: " ++ l
            Right ([], a) => go (a::acc) xs
            Right (s, _)  => Left $ "Leftover on line: " ++ pack s

pNat : Parser (List Char) Nat
pNat = MkParser $ \s =>
    let some = takeWhile isDigit s
        rest = drop (length some) s
    in case parseInteger (pack some) of
           Just n  => Right (rest, cast n)
           Nothing => Left "Not a Nat"

pItem : Eq a => a -> Parser (List a) ()
pItem x = MkParser $ \s =>
    case s of
        [] => Left "Out of input"
        (y::ys) =>
            if x == y
               then Right (ys, ())
               else Left "Mismatch"

-----------------------------------------------

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

-----------------------------------------------

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
