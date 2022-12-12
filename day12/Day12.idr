module Main

import System.File.ReadWrite

main : IO ()
main =

    case !(readFile "./day12/sample_input") of

        Left l =>
            putStrLn "Could not read input file"

        Right contents =>
            putStrLn contents
