module Main

import Common.Parser

import Data.String
import Data.SortedMap
import Debug.Trace
import System.File.ReadWrite

data BinOp =
    Plus | Times

Show BinOp where
    show Plus  = "+"
    show Times = "*"

data Arg =
    Old | N Int

Show Arg where
    show Old = "old"
    show (N n) = show n

pArg : Parser (List Char) Arg
pArg = (Old      <$  pString "old")
   <|> (N . cast <$> pNat)

data Expr =
    MkExpr Arg BinOp Arg

Show Expr where
    show (MkExpr a op b) = "(" ++ unwords [show a, show op, show b] ++ ")"

record Monkey where
    constructor MkMonkey
    n          : Nat
    startItems : (List Int)
    expr       : Expr
    divTest    : Nat
    throwTrue  : Nat
    throwFalse : Nat

parseBinOp : Parser (List Char) BinOp
parseBinOp = (Plus  <$ pItem '+')
         <|> (Times <$ pItem '*')

Show Monkey where
    show (MkMonkey n ss ex dt ttr tfl) =
        unwords [ "monkey", show n, show ss
                , show ex, show dt, show ttr
                , show tfl ]

parseMonkey : Parser (List Char) Monkey
parseMonkey = do
    n          <- pString "Monkey " *> pNat <* pRestOfLine
    startItems <- pString "  Starting items: " *> pSepBy (pString ", ") (cast <$> pNat) <* pRestOfLine
    expr       <- parseExpr
    divTest    <- pString "  Test: divisible by " *> pNat <* pRestOfLine
    throwTrue  <- pString "    If true: throw to monkey " *> pNat <* pRestOfLine
    throwFalse <- pString "    If false: throw to monkey " *> pNat <* pRestOfLine
    _          <- pMaybe (pItem '\n')
    pure $ MkMonkey n startItems expr divTest throwTrue throwFalse

    where
    parseExpr : Parser (List Char) Expr
    parseExpr = do
        _ <- pString "  Operation: new ="
        a <- pItem ' ' *> pArg
        o <- pItem ' ' *> parseBinOp
        b <- pItem ' ' *> pArg
        _ <- pRestOfLine
        pure $ MkExpr a o b

evalExpr : Int -> Expr -> Int
evalExpr old (MkExpr a o b) =
    let a': Int = case a of
                    N i => i
                    Old => old
        b': Int = case b of
                    N i => i
                    Old => old
    in case o of
        Plus  => a' + b'
        Times => a' * b'

part1 : SortedMap Nat Monkey -> IO ()
part1 = round 0
    where
    round : Nat -> SortedMap Nat Monkey -> IO ()
    round r = step 0
        where
        step : Nat -> SortedMap Nat Monkey -> IO ()
        step t ms =
            case lookup t ms of
                Nothing => putStrLn $ "Finished round " ++ show r
                Just m  => do
                    putStrLn "Monkey"
                    traverse_ (item m) m.startItems
                    step (S t) ms
            where
            item : Monkey -> Int -> IO ()
            item m iw = do

                let new      = evalExpr iw m.expr
                    relieved = div new 3

                putStr "  "
                print iw
                putStr " "
                print new
                putStr " "
                printLn relieved

main : IO ()
main =

    case !(readFile "./day11/sample_input") of

        Left l =>
            putStrLn "Could not read input file"

        Right contents =>

            case runParser (pMany parseMonkey) (unpack contents) of

                Left l =>
                    putStrLn $ "Did not parse input correctly: " ++ l

                Right (_, monkeys) =>
                    part1 . fromList $ map (\m => (m.n, m)) monkeys

                Right _ =>
                    putStrLn "Did not parse input correctly"

