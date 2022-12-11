module Main

import Common.Parser
import Data.String

import Debug.Trace
import System.File.ReadWrite

data BinOp =
    Plus | Times

Show BinOp where
    show Plus  = "+"
    show Times = "*"

data Arg =
    Old | N Nat

Show Arg where
    show Old = "old"
    show (N n) = show n

pArg : Parser (List Char) Arg
pArg = (Old <$  pString "old")
   <|> (N   <$> pNat)

data Expr =
    MkExpr Arg BinOp Arg

Show Expr where
    show (MkExpr a op b) = "(" ++ unwords [show a, show op, show b] ++ ")"

record Monkey where
    constructor MkMonkey
    n          : Nat
    startItems : (List Nat)
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
    startItems <- pString "  Starting items: " *> pSepBy (pString ", ") pNat <* pRestOfLine
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

main : IO ()
main =

    case !(readFile "./day11/sample_input") of

        Left l =>
            putStrLn "Could not read input file"

        Right contents =>

            case runParser (pMany parseMonkey) (unpack contents) of

                Left l =>
                    putStrLn $ "Did not parse input correctly: " ++ l

                Right (_, content) => do
                    traverse_ printLn content

                Right _ =>
                    putStrLn "Did not parse input correctly"

