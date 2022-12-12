module Main

import Data.SortedMap as M
import Data.SortedSet as S
import Data.String
import System.File.ReadWrite

data Elev =
    MkElev Int

Eq Elev where
    MkElev a == MkElev b = a == b

Show Elev where
    show (MkElev e) = show e

record Pos where
    constructor MkPos
    row : Int
    col : Int

Eq Pos where
    MkPos a b == MkPos c d = (a, b) == (c, d)

Ord Pos where
    MkPos a b < MkPos c d = (a, b) < (c, d)

Show Pos where
    show (MkPos row col) = concat [ "(", show row, ", ", show col, ")" ]

record Agent where
    constructor MkAgent
    pos  : Pos
    elev : Elev

fromChar : Char -> Maybe Elev
fromChar 'S' = Just . MkElev $ ord 'a' - 1
fromChar 'E' = Just . MkElev $ ord 'z' + 1
fromChar   c =
    if isLower c
        then Just . MkElev $ ord c
        else Nothing

data Start =
    MkStart Pos

Show Start where
    show (MkStart pos) = show pos

data End =
    MkEnd Pos

Show End where
    show (MkEnd pos) = show pos

buildGrid : List (List Char) -> SortedMap Pos Elev
buildGrid css = M.fromList . mapMaybe (\(p,c) => fromChar c <&> \e => (p, e)) . concat $ zipWith f [0..length css] css
    where
    f : Nat -> List Char -> List (Pos, Char)
    f rowN row = zipWith (\colN, cell => (MkPos (cast rowN) (cast colN), cell)) [0..length row] row

navigate : SortedMap Pos Elev -> Start -> End -> IO ()
navigate grid (MkStart s) end = do

    let unvisited = S.fromList
                  . filter (/= s)
                  . map fst
                  $ M.toList grid

    case M.lookup s grid of
        Nothing   => putStrLn "Couldn't place agent"
        Just elev => go unvisited (MkAgent s elev)

    where
    go : SortedSet Pos -> Agent -> IO ()
    go unvisited agent =

        case candidates agent.elev of
            []      => putStrLn "No more candidates"
            (c::cs) => do
                putStrLn "Candidates"
                printLn (c::cs)

        where
        candidates : Elev -> List (Pos, Elev)
        candidates (MkElev elev) =
            let row = agent.pos.row
                col = agent.pos.col
            in mapMaybe check [ MkPos row (col-1)
                              , MkPos row (col+1)
                              , MkPos (row-1) col
                              , MkPos (row+1) col ]
            where
            check : Pos -> Maybe (Pos, Elev)
            check x =
                if S.contains x unvisited
                    then case M.lookup x grid of
                             Nothing             => Nothing
                             Just (MkElev elev') =>
                                 if elev' == elev + 1 || elev' <= elev
                                     then Just (x, MkElev elev')
                                     else Nothing
                    else Nothing

part1 : SortedMap Pos Elev -> IO ()
part1 grid = do
    let cells = M.toList grid
    case fst <$> find (\(_, v) => Just v == fromChar 'S') cells of
        Nothing => putStrLn "No start"
        Just s  =>
            case fst <$> find (\(_, v) => Just v == fromChar 'E') cells of
                Nothing => putStrLn "No end"
                Just e  => navigate grid (MkStart s) (MkEnd e)

main : IO ()
main =

    case !(readFile "./day12/sample_input") of

        Left l =>
            putStrLn "Could not read input file"

        Right contents => do
            let grid = buildGrid . map unpack $ lines contents
            part1 grid
