module Main

import Data.String
import System.File.ReadWrite

data Move = Rock | Paper | Scissors

data Winner = P1 | P2 | Tie

data Round =
    MkRound Move Move Winner

parseMove1 : String -> Maybe Move
parseMove1 x =
  case x of
    "A" => Just Rock
    "X" => Just Rock
    "B" => Just Paper
    "Y" => Just Paper
    "C" => Just Scissors
    "Z" => Just Scissors
    _   => Nothing

winner : Move -> Move -> Winner
winner Rock Paper     = P2
winner Rock Scissors  = P1
winner Paper Rock     = P1
winner Paper Scissors = P2
winner Scissors Rock  = P2
winner Scissors Paper = P1
winner _        _     = Tie

score : Round -> Int
score (MkRound a b w) = selectScore b + winScore w
    where
    selectScore : Move -> Int
    selectScore Rock     = 1
    selectScore Paper    = 2
    selectScore Scissors = 3
    winScore : Winner -> Int
    winScore P1  = 0
    winScore P2  = 6
    winScore Tie = 3

part1 : String -> Int
part1 = sum . map score . mapMaybe parseLine1 . lines
  where
  parseLine1 line =
      case words line of
          [a, b] => do a' <- parseMove1 a
                       b' <- parseMove1 b
                       pure $ MkRound a' b' (winner a' b')
          _      => Nothing

parseChoice : Move -> String -> Maybe Move
parseChoice a b =
    case b of
        "X" => Just $ loseTo a
        "Y" => Just a
        "Z" => Just $ beat a
        _   => Nothing

    where
    beat : Move -> Move
    beat Rock     = Paper
    beat Paper    = Scissors
    beat Scissors = Rock

    loseTo : Move -> Move
    loseTo Rock     = Scissors
    loseTo Paper    = Rock
    loseTo Scissors = Paper

part2 : String -> Int
part2 = sum . map score . mapMaybe parseLine2 . lines
  where
  parseLine2 line =
      case words line of
          [a, b] => do a' <- parseMove1 a
                       b' <- parseChoice a' b
                       pure $ MkRound a' b' (winner a' b')
          _      => Nothing

main : IO ()
main =

    case !(readFile "./day02/input") of

        Left l =>
            putStrLn "Could not read input file"

        Right contents => do
            printLn $ part1 contents
            printLn $ part2 contents
