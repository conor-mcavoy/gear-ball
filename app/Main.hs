module Main (main) where

import Lib

main :: IO ()
main = do
    putStrLn "Welcome to Gear Ball!"
    putStrLn "====================="
    putStrLn $ "Solved ball reports solved: " ++ show (isSolved solvedGearBall)
    case parseMoves "R R R R R R R R R R R R" of
        Just moves -> putStrLn $ "12x R returns to solved: " ++ show (isSolved (applyMoves solvedGearBall moves))
        Nothing    -> putStrLn "Failed to parse demo moves"
