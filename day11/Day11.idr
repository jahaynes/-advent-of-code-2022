module Main

import Common.Monad
import Common.Parser
import Common.State

import Data.List as L
import Data.String
import Data.SortedMap as M
import System.File.ReadWrite

data BinOp =
    Plus | Times

data Arg =
    Old | N Integer

pArg : Parser (List Char) Arg
pArg = (Old      <$  pString "old")
   <|> (N . cast <$> pNat)

data Expr =
    MkExpr Arg BinOp Arg

record Monkey where
    constructor MkMonkey
    n          : Nat
    startItems : (List Integer)
    expr       : Expr
    divTest    : Integer
    throwTrue  : Nat
    throwFalse : Nat

data Inspection =
    MkInspection Int (SortedMap Nat Integer)

parseBinOp : Parser (List Char) BinOp
parseBinOp = (Plus  <$ pItem '+')
         <|> (Times <$ pItem '*')

parseMonkey : Parser (List Char) Monkey
parseMonkey = do
    n          <- pString "Monkey " *> pNat <* pRestOfLine
    startItems <- pString "  Starting items: " *> pSepBy (pString ", ") (cast <$> pNat) <* pRestOfLine
    expr       <- parseExpr
    divTest    <- pString "  Test: divisible by " *> (cast <$> pNat) <* pRestOfLine
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

evalExpr : Integer -> Expr -> Integer
evalExpr old (MkExpr a o b) =
    let a' = from a
        b' = from b
    in case o of
        Plus  => a' + b'
        Times => a' * b'
    where
    from : Arg -> Integer
    from (N i) = i
    from Old   = old

throwFromTo : Integer -> Nat -> Nat -> SortedMap Nat Monkey -> Maybe (SortedMap Nat Monkey)
throwFromTo w i1 i2 ms = do
    m1 <- lookup i1 ms
    m2 <- lookup i2 ms
    case m1.startItems of
        []      => Nothing
        (_::ws) =>
            let m1' : Monkey = { startItems := ws } m1
                m2' : Monkey = { startItems := m2.startItems ++ [w] } m2
            in Just . insert m2'.n m2'
                    . insert m1'.n m1'
                    $ ms

monkeyInspectedItem : Nat -> State Inspection ()
monkeyInspectedItem mn = do
    MkInspection r ms <- get
    let ms' = case lookup mn ms of
                  Nothing => insert mn     1 ms
                  Just c  => insert mn (c+1) ms
    put $ MkInspection r ms'

run : Nat -> (Integer -> Integer) -> SortedMap Nat Monkey -> Maybe Integer
run nRounds relax monkeys =

    let MkInspection _ inspection = fst . runState (rounds nRounds monkeys) $ MkInspection 0 empty

    in case L.take 2 . reverse . sort . map snd $ M.toList inspection of
           [a, b] => Just $ a * b
           _      => Nothing

    where
    rounds : Nat -> SortedMap Nat Monkey -> State Inspection (SortedMap Nat Monkey)
    rounds = iterateM (step 0)
        where
        step : Nat -> SortedMap Nat Monkey -> State Inspection (SortedMap Nat Monkey)
        step t ms =
            case lookup t ms of
                Nothing => pure ms
                Just m  => step (S t) =<< foldlM (item m) ms m.startItems
            where
            item : Monkey -> SortedMap Nat Monkey -> Integer -> State Inspection (SortedMap Nat Monkey)
            item m ms iw = do
                monkeyInspectedItem m.n
                let new      = evalExpr iw m.expr
                    relieved = relax new
                    other    = if mod relieved m.divTest == 0
                                   then m.throwTrue
                                   else m.throwFalse
                pure $ case throwFromTo relieved m.n other ms of
                           Just ms' => ms'
                           Nothing  => ms

part1 : SortedMap Nat Monkey -> Maybe Integer
part1 = run 20 (\w => div w 3)

part2 : SortedMap Nat Monkey -> Maybe Integer
part2 monkeys = do
    let divisor = product $ map (.divTest) monkeys
    run 10000 (`mod` divisor) monkeys

main : IO ()
main =

    case !(readFile "./day11/input") of

        Left l =>
            putStrLn "Could not read input file"

        Right contents =>

            case runParser (pMany parseMonkey) (unpack contents) of

                Left l =>
                    putStrLn $ "Did not parse input correctly: " ++ l

                Right (_, monkeys) => do
                    let monkeys' = fromList $ map (\m => (m.n, m)) monkeys
                    printLn $ part1 monkeys'
                    printLn $ part2 monkeys'

                Right _ =>
                    putStrLn "Did not parse input correctly"

