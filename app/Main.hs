module Main where

import Lib
import Syntax

main :: IO ()
main = do
    let exp1' = trans exp1
        exp2' = trans exp2
    putStr "exp1  = "
    putStrLn $ pshow exp1
    putStr "      = "
    print $ eval exp1
    putStr "exp1' = "
    putStrLn $ pshow exp1'
    putStr "      = "
    print $ eval exp1'
    putStr "exp2  = "
    putStrLn $ pshow exp2
    putStr "      = "
    print $ eval exp2
    putStr "exp2' = "
    putStrLn $ pshow exp2'
    putStr "      = "
    print $ eval exp2'
