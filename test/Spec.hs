module Main (main) where

import Prelude hiding (Left, Right)
import Lib
import Search
import Ergonomics
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Data.List (nub)
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

-- The two-pair edge swap, specified about the R-L axis: FR<->BR and FL<->BL.
twoPairSwapAboutRL :: BallState -> Bool
twoPairSwapAboutRL st = all (\m -> edgeRelocated m start st) maps && preserves keep start st
  where
    start = toBallState solvedGearBall
    moved = [edgeSlot Front Right, edgeSlot Back Right, edgeSlot Front Left, edgeSlot Back Left]
    maps = [ [(Front, Back), (Right, Right)], [(Back, Front), (Right, Right)]
           , [(Front, Back), (Left, Left)],   [(Back, Front), (Left, Left)] ]
    keep = Set.fromList (map faceVec allFaceLocations)
             `Set.union` (Set.fromList allEdgeSlots `Set.difference` Set.fromList moved)

moves :: String -> [Move]
moves s = [ (f, p) | Turn f p n <- turns s, _ <- [1 .. n] ]

-- The eight algorithms covering the last-slice endgame: two edge slices,
-- centers and every gear already solved, with the last slice's four edges and
-- the corners outstanding. Case 1 of the nine is "already solved", so eight
-- algorithms cover the lot. See ALGORITHMS.md for the case table.
lastSliceAlgs :: [String]
lastSliceAlgs =
  [ "R' U R B R D' R' B'"     -- edges solved, corners half-turned
  , "L' U' L U L F R' F'"     -- straight swap, corners double-transposed
  , "L' U' F L U B'"          -- straight swap, corners quadruple-transposed
  , "L U L U' L' B' R' B"     -- straight swap, the other double
  , "L U B' R' U' B"          -- straight swap, the other quadruple
  , "U L F L F' L' U' R'"     -- diagonal swap, corners double-transposed
  , "U' L' B' U R B"          -- diagonal swap, corners quadruple-transposed
  , "U L F U' R' F'"          -- diagonal swap, the other quadruple
  ]

-- The two slices that are already solved: every edge slot off the L-R axis.
solvedSliceSlots :: [Vec3]
solvedSliceSlots = [ v | v@(x, _, _) <- allEdgeSlots, x /= 0 ]

-- Spin-neutral: no gear anywhere has been turned.
allGearsZero :: BallState -> Bool
allGearsZero st = all (\v -> snd (bsEdges st Map.! v) == Zero) allEdgeSlots

-- The state an algorithm is meant to fix, obtained by running it backwards
-- from solved.
caseFor :: String -> GearBall
caseFor a =
  fromBallState (foldl (\s (f, p) -> applyBaseTurn f p s)
                       (toBallState solvedGearBall)
                       (reverse (map (\(f, p) -> (f, not p)) (moves a))))

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
    , check "searchAll's first result is exactly what search returns"
        (let p st = matches fullySolved st
         in take 1 (searchAll 3 allMoves p (run "R")) == maybe [] (: []) (search 3 allMoves p (run "R")))
    , check "searchAll finds all 64 six-move two-pair swaps about the R-L axis"
        (length (searchAll 6 allMoves twoPairSwapAboutRL solvedGearBall) == 64)

    -- Ergonomics. The scorer never searches; it ranks what the searcher emitted.
    , check "there are 24 distinct holds" (length allHolds == 24)
    , check "an all-R algorithm costs 1 for the first turn and 2 per repeat"
        (exCost (score (moves "R R R")) == 1 + 2 + 2)
    , check "U costs 1.5 and the awkward faces cost 2"
        (map (exCost . score . moves) ["R", "L", "U", "D", "F", "B"] == [1, 1, 1.5, 2, 2, 2])
    , check "a lone F cannot be improved - no R/L turn precedes it to re-hold on"
        (renderExecution (score (moves "F")) == "F")
    , check "R' F is executed as R' x U, turning the awkward F into a U"
        (renderExecution (score (moves "R' F")) == "R' x U")
    , check "and that re-hold is what makes it cheaper than the literal 1 + 2"
        (exCost (score (moves "R' F")) == 1 + 1.5)
    , check "every execution of every two-pair-swap candidate really does the job"
        (let cands = searchAll 6 allMoves twoPairSwapAboutRL solvedGearBall
             start = toBallState solvedGearBall
         in and [ verifyExecution ms ex start
                | ms <- cands, ex <- executions identityHold ms ])
    , check "the best-scoring six-move two-pair swap about R-L scores 8.5"
        (let cands = searchAll 6 allMoves twoPairSwapAboutRL solvedGearBall
         in minimum (map (exCost . score) cands) == 8.5)
    , check "no R/L/U-only two-pair swap exists within 8 moves"
        (null (searchAll 8 (movesFor [Right, Left, Up]) twoPairSwapAboutRL solvedGearBall))

    -- Diagonal edge swap: FU<->BD and FD<->BU, each edge crossing to the slot
    -- antipodal to it, with every gear left as it was found.
    , check "the diagonal edge swap does exactly what it claims, gears included"
        (let start = toBallState solvedGearBall
             end = toBallState (run "U L F U' R' F'")
             maps = [ [(Front, Back), (Up, Down)], [(Back, Front), (Down, Up)]
                    , [(Front, Back), (Down, Up)], [(Back, Front), (Up, Down)] ]
             moved = [edgeSlot Front Up, edgeSlot Back Down, edgeSlot Front Down, edgeSlot Back Up]
             keep = Set.fromList (map faceVec allFaceLocations)
                      `Set.union` (Set.fromList allEdgeSlots `Set.difference` Set.fromList moved)
         in all (\m -> edgeRelocated m start end) maps && preserves keep start end)
    , check "the diagonal edge swap is an involution"
        (isSolved (applyMoves (run "U L F U' R' F'") (turns "U L F U' R' F'")))
    , check "6 moves is optimal for the diagonal edge swap"
        (let start = toBallState solvedGearBall
             maps = [ [(Front, Back), (Up, Down)], [(Back, Front), (Down, Up)]
                    , [(Front, Back), (Down, Up)], [(Back, Front), (Up, Down)] ]
             moved = [edgeSlot Front Up, edgeSlot Back Down, edgeSlot Front Down, edgeSlot Back Up]
             keep = Set.fromList (map faceVec allFaceLocations)
                      `Set.union` (Set.fromList allEdgeSlots `Set.difference` Set.fromList moved)
             ok st = all (\m -> edgeRelocated m start st) maps && preserves keep start st
         in all ((== 6) . length) (searchAll 6 allMoves ok solvedGearBall))
    , check "it scores 8.5, performed as U L F U' R' x U'"
        (let ex = score (moves "U L F U' R' F'")
         in exCost ex == 8.5 && renderExecution ex == "U L F U' R' x U'")
    , check "and no re-hold beats holding it exactly as written"
        (exCost (snd (bestHold (moves "U L F U' R' F'"))) == 8.5)

    -- The corner-only algorithm, chosen for ergonomics out of the 12,288
    -- 8-move ones. It must leave every edge and center byte-identical.
    , check "the recommended corner algorithm touches no edge and no center"
        (let start = toBallState solvedGearBall
             keep = Set.fromList (allEdgeSlots ++ map faceVec allFaceLocations)
         in preserves keep start (toBallState (run "R' U R B R D' R' B'")))
    , check "it moves the corners (it is not a no-op)"
        (not (isSolved (run "R' U R B R D' R' B'")))
    , check "it is an involution"
        (isSolved (applyMoves (run "R' U R B R D' R' B'") (turns "R' U R B R D' R' B'")))
    , check "it executes as R' U R x' U R x' U' R' x U', touching only R and U"
        (renderExecution (score (moves "R' U R B R D' R' B'")) == "R' U R x' U R x' U' R' x U'")
    , check "that execution scores 10.0, beating the plainer L F' U B' L' B U' F at 13.0"
        (exCost (score (moves "R' U R B R D' R' B'")) == 10.0
          && exCost (score (moves "L F' U B' L' B U' F")) == 13.0)
    , check "and it really is equivalent to the written algorithm"
        (verifyExecution (moves "R' U R B R D' R' B'")
                         (score (moves "R' U R B R D' R' B'"))
                         (toBallState solvedGearBall))

    -- The last-slice endgame. Each algorithm must be spin-neutral and must
    -- leave the two already-solved slices and the centers exactly alone.
    , check "every last-slice algorithm leaves all 12 gears unspun"
        (all (allGearsZero . toBallState . run) lastSliceAlgs)
    , check "every last-slice algorithm leaves the two solved slices and centers alone"
        (let start = toBallState solvedGearBall
             keep = Set.fromList (map faceVec allFaceLocations ++ solvedSliceSlots)
         in all (\a -> preserves keep start (toBallState (run a))) lastSliceAlgs)
    , check "each last-slice algorithm solves the case it is for"
        (all (\a -> isSolved (applyMoves (caseFor a) (turns a))) lastSliceAlgs)
    , check "the 8 last-slice cases are pairwise distinct"
        (length (nub (map caseFor lastSliceAlgs)) == 8)
    , check "and none of those 8 cases is already solved"
        (not (any (isSolved . caseFor) lastSliceAlgs))
    , check "no last-slice algorithm is longer than 8 moves"
        (all ((<= 8) . length . moves) lastSliceAlgs)

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
