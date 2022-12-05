module Main

import Common.Monad
import Common.Parser

import Data.List
import System.File.ReadWrite

data Part = Part1 | Part2

data Slot = Empty
          | Block Char

data Line = Slots (List Slot)
          | MovInstr Nat Nat Nat
          | Other

parseSlot : Parser (List Char) Slot
parseSlot = Empty <$  replicateM_ 3 (pItem ' ')
        <|> Block <$> (pItem '[' *> pNext <* pItem ']')

parseLine : Parser (List Char) Line
parseLine = Slots    <$> pMany1 (parseSlot <* pItemMaybe ' ')
        <|> MovInstr <$> (pString "move " *> pNat) <*> (pString " from " *> pNat) <*> (pString (" to ") *> pNat)
        <|> Other    <$  pDropWhile (const True)

prepare : List Line -> (List (List Slot), List (Nat, Nat, Nat))
prepare = go ([], [])
    where
    go : (List (List Slot), List (Nat, Nat, Nat)) -> List Line -> (List (List Slot), List (Nat, Nat, Nat))
    go     (slots, instrs)      [] = (reverse slots, reverse instrs)
    go acc@(slots, instrs) (x::xs) =
        case x of
            Slots s                => go (s::slots,          instrs) xs
            MovInstr a (S f) (S t) => go (   slots, (a,f,t)::instrs) xs
            _                      => go                         acc xs

update : Nat -> (a -> (b, a)) -> List a -> (Maybe b, List a)
update n f = go 0 Nothing []
    where
    go : Nat -> Maybe b -> List a -> List a -> (Maybe b, List a)
    go _ mOld acc      [] = (mOld, reverse acc)
    go m mOld acc (x::xs) =
        if m == n
            then let (a, b) = f x in go (S m) (Just a) (b::acc) xs
            else                     go (S m)     mOld (x::acc) xs

process : Part -> List Line -> String
process part lines =

    let (rows, instrs) = prepare lines

        columns = map (dropWhile isEmpty)
                . transpose
                . filter (not . null)
                $ rows

        topRow = pack . mapMaybe extract
               . mapMaybe head'
               $ foldl runStep columns instrs

    in topRow

    where
    isEmpty : Slot -> Bool
    isEmpty Empty     = True
    isEmpty (Block _) = False

    extract : Slot -> Maybe Char
    extract (Block c) = Just c
    extract _         = Nothing

    runStep : List (List Slot) -> (Nat, Nat, Nat) -> List (List Slot)
    runStep columns (amt, from, to) = do

        let (mYoink, columns') =
            update from (\c => (take amt c, drop amt c)) columns

        case mYoink of
            Nothing    => columns
            Just yoink =>

                let yoink' =
                    case part of
                        Part1 => reverse yoink
                        Part2 => yoink

                    (mDone, columns'') = update to (\c => ((), yoink' ++ c)) columns'

                in columns''

main : IO ()
main =

    case !(readFile "./day05/input") of

        Left l =>
            putStrLn "Could not read input file"

        Right contents =>

            case runParser (pLinesOf parseLine) contents of

                Left l =>
                    putStrLn $ "Did not parse input correctly: " ++ l

                Right ("", xs) => do
                    putStrLn $ process Part1 xs
                    putStrLn $ process Part2 xs

                Right _ =>
                    putStrLn "Did not parse input correctly"