module Common.Parser

import Data.String

export
data Parser s a =
    MkParser (s -> Either String (s, a))

export
runParser : Parser s a -> s -> Either String (s, a)
runParser (MkParser run) s = run s

export
Functor (Parser s) where

    map f (MkParser run) = MkParser $ \s =>
        case run s of
            Right (s', a) => Right (s', f a)
            Left l        => Left l

export
Applicative (Parser s) where

    pure x = MkParser $ \s => Right (s, x)

    MkParser runf <*> MkParser runx = MkParser $ \s =>
        case runf s of
            Left l => Left l
            Right (s', f) =>
                case runx s' of
                    Left l => Left l
                    Right (s'', x) => Right (s'', f x)

export
Monad (Parser s) where

    MkParser runa >>= f = MkParser $ \s =>
        case runa s of
            Left l => Left l
            Right (s', a) =>
                let MkParser r = f a
                in r s'

export
Alternative (Parser s) where

    empty = MkParser $ \_ => Left "no more alternatives"

    MkParser runp <|> MkParser runq = MkParser $ \s =>
      case runp s of
        Right r => Right r
        Left  _ =>
          case runq s of
            Right r => Right r
            Left  _ => Left "no matches"

export
pFail : String -> Parser s a
pFail msg = MkParser $ \s => Left msg

export
pNext : Parser (List a) a
pNext = MkParser $ \s =>
    case s of
        []      => Left "Not enough input"
        (x::xs) => Right (xs, x)

export
pTake : Nat -> Parser (List Char) (List Char)
pTake n = MkParser $ \s =>
    let (some, rest) = splitAt n s
    in if length some < n
        then Left "Not enough input"
        else Right (rest, some)

export
pTakeWhile : (s -> Bool) -> Parser (List s) (List s)
pTakeWhile p = MkParser $ \s =>
    let some = takeWhile p s
    in Right (drop (length some) s, some)

export
pDropWhile : (s -> Bool) -> Parser (List s) ()
pDropWhile p = MkParser $ \s =>
    Right (dropWhile p s, ())

export
pLinesOf : Parser (List Char) a
        -> Parser String (List a)
pLinesOf (MkParser run) = MkParser $ go [] . lines
    where
    go : List a
      -> List String
      -> Either String (String, List a)
    go acc [] = Right ("", reverse acc)
    go acc (x::xs) =
        case run (unpack x) of
            Left l        => Left $ "Within pLinesOf: " ++ l
            Right ([], a) => go (a::acc) xs
            Right (s, _)  => Left $ "Leftover on line: \"" ++ pack s ++ "\""

export
pNat : Parser (List Char) Nat
pNat = MkParser $ \s =>
    let some = takeWhile isDigit s
        rest = drop (length some) s
    in case parseInteger (pack some) of
           Just n  => Right (rest, cast n)
           Nothing => Left "Not a Nat"

export
pItem : Eq a => a -> Parser (List a) ()
pItem x = MkParser $ \s =>
    case s of
        [] => Left "Out of input"
        (y::ys) =>
            if x == y
               then Right (ys, ())
               else Left "Mismatch"

export
pList : Eq a => List a -> Parser (List a) ()
pList xs = MkParser $ \s =>
    let len  = length xs
        some = take len s
    in if some == xs
           then Right (drop len s, ())
           else Left "Mismatch"

export
pString : String -> Parser (List Char) ()
pString = pList . unpack

export
pItemMaybe : Eq a => a -> Parser (List a) (Maybe ())
pItemMaybe x = MkParser $ \s =>
    case s of
        [] => Right (s, Nothing)
        (y::ys) =>
            if x == y
               then Right (ys, Just ())
               else Right (s, Nothing)

export
pMany : Parser s b -> Parser s (List b)
pMany (MkParser runp) = MkParser $ go []
    where
    go : List b -> s -> Either String (s, List b)
    go acc s =
        case runp s of
            Left l        => Right (s, reverse acc)
            Right (s', x) => go (x::acc) s'

export
pMany1 : Parser s a -> Parser s (List a)
pMany1 p = do
  xs <- pMany p
  if null xs
    then pFail "none found for pMany1"
    else pure xs
