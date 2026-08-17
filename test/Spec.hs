module Main (main) where

import Lib
import System.Exit (exitFailure)

turns :: String -> [Turn]
turns s = case parseMoves s of
  Just ts -> ts
  Nothing -> error ("failed to parse moves: " ++ s)

run :: String -> GearBall
run s = applyMoves solvedGearBall (turns s)

check :: String -> Bool -> IO Bool
check name ok = do
  putStrLn ((if ok then "PASS: " else "FAIL: ") ++ name)
  return ok

main :: IO ()
main = do
  results <- sequence
    [ check "solved ball reports solved" (isSolved solvedGearBall)
    , check "a single R turn leaves the ball unsolved" (not (isSolved (run "R")))
    , check "12 consecutive R turns return to solved" (isSolved (run "R R R R R R R R R R R R"))
    , check "R2 six times (12 turns) returns to solved" (isSolved (run "R2 R2 R2 R2 R2 R2"))
    , check "R then R' returns to solved (R' is a true inverse)" (isSolved (run "R R'"))
    , check "6 R turns equal 6 R' turns" (run "R R R R R R" == run "R' R' R' R' R' R'")
    , check "R2 equals R applied twice" (run "R2" == run "R R")
    , check "every face's letter returns to solved after 12 uses"
        (all (\f -> isSolved (run (unwords (replicate 12 [f])))) "RLUDFB")
    , check "every face's letter returns to solved after 6 uses plus 6 primed uses"
        (all (\f -> isSolved (run (unwords (replicate 6 [f] ++ replicate 6 [f, '\''])))) "RLUDFB")
    , check "R L' reorients the ball without scrambling it" (isSolvedUpToReorientation (run "R L'"))
    , check "R L' is not literally the original orientation" (not (isSolved (run "R L'")))
    , check "invalid notation fails to parse" (parseMoves "Q" == Nothing)
    , check "out-of-range repeat count fails to parse" (parseMoves "R7" == Nothing)
    ]
  if and results
    then putStrLn "All tests passed."
    else do
      putStrLn "Some tests failed."
      exitFailure
