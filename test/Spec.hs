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

-- Fixes a "yellow ring" that's otherwise correctly homed except the
-- Red-Yellow and Purple-Yellow edges are swapped with each other - anchored
-- to color identity throughout, so it doesn't matter which absolute face
-- Yellow ended up on or what else is scrambled elsewhere.
yellowRingFix :: GearBall -> Maybe [Move]
yellowRingFix broken = search 4 allMoves isSolution broken
  where
    start = toBallState broken
    isFixed st =
      edgeHome Green Yellow st && edgeHome Blue Yellow st
      && edgeHome Red Yellow st && edgeHome Purple Yellow st
    protectSlots = Set.fromList
      ( map faceVec allFaceLocations
        ++ [edgeWithColors Green Yellow start, edgeWithColors Blue Yellow start]
      )
    isSolution st = isFixed st && preserves protectSlots start st

-- Fixes a genuine FR/FL edge swap: F stays F, R becomes L (and vice versa) -
-- purely positional, no color lookups at all. Protects FU, FD, and every
-- center exactly as they started. This is the "purely about moving pieces
-- around" reading of "swap FR and FL" - edgeRelocated states the
-- correspondence explicitly rather than inferring it from a shared face.
frFlSwapFix :: GearBall -> Maybe [Move]
frFlSwapFix broken = search 8 allMoves isSolution broken
  where
    start = toBallState broken
    frToFl = [(Front, Front), (Right, Left)]
    flToFr = [(Front, Front), (Left, Right)]
    protectSlots = Set.fromList
      (map faceVec allFaceLocations ++ [edgeSlot Front Up, edgeSlot Front Down])
    isSolution st =
      edgeRelocated frToFl start st && edgeRelocated flToFr start st
      && preserves protectSlots start st

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
        (searchFor 0 allMoves Set.empty fullySolved solvedGearBall == Just [])
    , check "search finds R' as the shortest fix for a single R turn"
        (searchFor 3 allMoves Set.empty fullySolved (run "R") == Just [(Right, True)])
    , check "restricting the moveset to U/D/F/B can't undo an R turn"
        (searchFor 6 (movesFor [Up, Down, Front, Back]) Set.empty fullySolved (run "R") == Nothing)
    , check "without protection, F' is the obvious fix for Up's center after F"
        (searchFor 1 allMoves Set.empty (solvedGoal [faceVec Up]) (run "F") == Just [(Front, True)])
    , check "protecting URF forces search away from F' to a different single-move fix"
        (searchFor 3 allMoves (Set.singleton (cornerSlot Up Right Front)) (solvedGoal [faceVec Up]) (run "F")
          == Just [(Back, False)])
    , check "withTouchingCenters pulls in an edge's two neighboring centers"
        (withTouchingCenters [edgeSlot Front Up]
          == Set.fromList [edgeSlot Front Up, faceVec Front, faceVec Up])
    , check "withTouchingCenters pulls in a corner's three neighboring centers"
        (withTouchingCenters [cornerSlot Up Right Front]
          == Set.fromList [cornerSlot Up Right Front, faceVec Up, faceVec Right, faceVec Front])
    , check "withTouchingCenters on a center slot is a no-op"
        (withTouchingCenters [faceVec Up] == Set.singleton (faceVec Up))
    , check "withTouchingCenters de-duplicates shared centers across slots"
        (withTouchingCenters [edgeSlot Front Up, edgeSlot Front Down]
          == Set.fromList [edgeSlot Front Up, edgeSlot Front Down, faceVec Front, faceVec Up, faceVec Down])
    , check "yellow-ring parity fix (L U L' B') works from one concrete broken scramble"
        (yellowRingFix (run "U L' F' L'") == Just [(Left, False), (Up, False), (Left, True), (Back, True)])
    , check "the same fix also resolves a completely different broken scramble"
        (yellowRingFix (run "U U R' F' L' D'") == Just [(Left, False), (Up, False), (Left, True), (Back, True)])
    , check "a genuine positional FR/FL swap (U2 F U2 F) needs more than 4 moves"
        (frFlSwapFix (run "U L' F' L'")
          == Just [(Up, False), (Up, False), (Front, False), (Up, False), (Up, False), (Front, False)])
    , check "edgeRelocated distinguishes FR-to-FL from the flipped FR-to-LF"
        (let start = toBallState (run "U L' F' L'")
             result = toBallState (applyMoves (run "U L' F' L'") (turns "U2 F U2 F"))
             frToFl = [(Front, Front), (Right, Left)]
             frToLf = [(Front, Left), (Right, Front)]
         in edgeRelocated frToFl start result && not (edgeRelocated frToLf start result))
    ]
  if and results
    then putStrLn "All tests passed."
    else do
      putStrLn "Some tests failed."
      exitFailure
