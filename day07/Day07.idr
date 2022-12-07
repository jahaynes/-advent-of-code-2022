module Main

import Common.Parser

import Data.List
import Data.SortedSet as S
import System.File.ReadWrite

data Line = Cd String
          | Ls
          | Dir String
          | SizedFile Nat String

Show Line where
    show           (Cd name) = "$ cd " ++ name
    show                  Ls = "$ ls"
    show          (Dir name) = "dir " ++ name
    show (SizedFile sz name) = show sz ++ " " ++ name

parseLine : Parser (List Char) Line
parseLine = Cd        <$> (pString "$ cd " *> pRest)
        <|> Ls        <$  (pString "$ ls")
        <|> Dir       <$> (pString "dir " *> pRest)
        <|> SizedFile <$> (pNat <* pItem ' ') <*> pRest

data Node = NPath (List String)
          | NFile Nat (List String)
          | NSzPath Nat (List String)

Show Node where
    show (NPath path)      = show path
    show (NFile sz path)   = show sz ++ " " ++ show path
    show (NSzPath sz path) = show sz ++ " " ++ show path

Eq Node where
    NPath a         == NPath b         = a == b
    NFile asz apath == NFile bsz bpath = (asz, apath) == (bsz, bpath)
    _               == _               = False

Ord Node where
    NPath a           < NPath b           = a < b
    NFile asz apath   < NFile bsz bpath   = (asz, apath) < (bsz, bpath)
    NSzPath asz apath < NSzPath bsz bpath = (asz, apath) < (bsz, bpath)
    a                 < b                 = show a < show b

buildTree : List Line -> SortedSet Node
buildTree = fst . foldl step (empty, Nothing)
    where
    step : (SortedSet Node, Maybe (List String))
        -> Line
        -> (SortedSet Node, Maybe (List String))
    step (acc,            _) (Cd "/")            = (acc, Just ["/"])
    step (acc, Just (p::wd)) (Cd "..")           = (acc, Just $ init (p::wd))
    step (acc, Just     pwd) (Cd path)           = (acc, Just $ pwd ++ [path])
    step (acc,         mPwd) Ls                  = (acc, mPwd)
    step (acc, Just     pwd) (Dir name)          = (insert (NPath $ pwd ++ [name]) acc, Just pwd)
    step (acc, Just     pwd) (SizedFile sz name) = (insert (NFile sz $ pwd ++ [name]) acc, Just pwd)
    step (acc, pwd) _                            = (acc, pwd)

part1 : List Line -> IO ()
part1 ls = do
    let tree  = buildTree ls
    traverse_ printLn . S.toList $ tree

main : IO ()
main =

    case !(readFile "./day07/sample_input") of

        Left l =>
            putStrLn "Could not read input file"

        Right contents =>

            case runParser (pLinesOf parseLine) contents of

                Left l =>
                    putStrLn $ "Did not parse input correctly: " ++ l

                Right ("", ls) => do
                    part1 ls

                Right _ =>
                    putStrLn "Did not parse input correctly"