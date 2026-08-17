module Main (main) where

import Prelude hiding (Left, Right)
import Lib
import Search
import qualified Data.Set as Set
import System.Exit (exitFailure)

turns :: String -> [Turn]
turns s = case parseMoves s of
  Just ts -> ts
  Nothing -> error ("failed to parse moves: " ++ s)

run :: String -> GearBall
run s = applyMoves solvedGearBall (turns s)

fullySolved :: Goal
fullySolved = solvedGoal (Set.toList allSlots)

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
    , check "search finds the trivial zero-move solution to an already-met goal"
        (search 0 allMoves Set.empty fullySolved solvedGearBall == Just [])
    , check "search finds R' as the shortest fix for a single R turn"
        (search 3 allMoves Set.empty fullySolved (run "R") == Just [(Right, True)])
    , check "restricting the moveset to U/D/F/B can't undo an R turn"
        (search 6 (movesFor [Up, Down, Front, Back]) Set.empty fullySolved (run "R") == Nothing)
    , check "without protection, F' is the obvious fix for Up's center after F"
        (search 1 allMoves Set.empty (solvedGoal [faceVec Up]) (run "F") == Just [(Front, True)])
    , check "protecting URF forces search away from F' to a different single-move fix"
        (search 3 allMoves (Set.singleton (cornerSlot Up Right Front)) (solvedGoal [faceVec Up]) (run "F")
          == Just [(Back, False)])
    ]
  if and results
    then putStrLn "All tests passed."
    else do
      putStrLn "Some tests failed."
      exitFailure
