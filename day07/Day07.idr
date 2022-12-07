module Main

import Common.Parser

import Data.List
import Data.SortedMap as M
import Data.SortedSet as S
import System.File.ReadWrite

data Line = Cd String
          | Ls
          | Dir String
          | SizedFile Nat String

parseLine : Parser (List Char) Line
parseLine = Cd        <$> (pString "$ cd " *> pRest)
        <|> Ls        <$  (pString "$ ls")
        <|> Dir       <$> (pString "dir " *> pRest)
        <|> SizedFile <$> (pNat <* pItem ' ') <*> pRest

data Node = NPath (List String)
          | NFile Nat (List String)

Show Node where
    show (NPath path)    = show path
    show (NFile sz path) = show sz ++ " " ++ show path

Eq Node where
    NPath a         == NPath b         = a == b
    NFile asz apath == NFile bsz bpath = (asz, apath) == (bsz, bpath)
    _               == _               = False

Ord Node where
    NPath a           < NPath b           = a < b
    NFile asz apath   < NFile bsz bpath   = (asz, apath) < (bsz, bpath)
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

buildPathSizes : SortedSet Node -> SortedMap (List String) Nat
buildPathSizes = foldl step empty . S.toList
    where
    step : SortedMap (List String) Nat -> Node -> SortedMap (List String) Nat
    step acc node =
        case node of
            NFile sz fullPath@(_::_) => let path = init fullPath in foldl (go sz) acc (drop 1 $ inits path)
            _                        => acc
        where
        go : Nat -> SortedMap p Nat -> p -> SortedMap p Nat
        go dsz acc path =
            let sz' = case lookup path acc of
                            Nothing => dsz
                            Just sz => dsz + sz
            in insert path sz' acc

part1 : SortedMap (List String) Nat -> Nat
part1 = sum
      . filter (<= 100000)
      . map snd
      . M.toList

part2 : SortedMap (List String) Nat -> Maybe Nat
part2 pathSizeMap = do

    let diskSize = 70000000

        maxSize = diskSize - 30000000

        pathSizes = sortBy (compare `on` snd)
                  $ M.toList pathSizeMap

    usedSize <- lookup ["/"] pathSizes

    case dropWhile (\(_, sz) => cast usedSize - cast sz > maxSize) pathSizes of
        []           => Nothing
        ((_, sz)::_) => Just sz

main : IO ()
main =

    case !(readFile "./day07/input") of

        Left l =>
            putStrLn "Could not read input file"

        Right contents =>

            case runParser (pLinesOf parseLine) contents of

                Left l =>
                    putStrLn $ "Did not parse input correctly: " ++ l

                Right ("", ls) => do
                    let pathSizes = buildPathSizes $ buildTree ls
                    printLn $ part1 pathSizes
                    printLn $ part2 pathSizes

                Right _ =>
                    putStrLn "Did not parse input correctly"
