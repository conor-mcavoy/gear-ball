module Search
    ( SlotValue(..)
    , slotValue
    , Goal(..)
    , goalFrom
    , solvedGoal
    , matches
    , preserves
    , allSlots
    , withTouchingCenters
    , centerSlotOf
    , edgeWithColors
    , cornerWithColors
    , edgeHome
    , cornerHome
    , Relabeling
    , edgeRelocated
    , cornerRelocated
    , search
    , searchFor
    ) where

import Prelude hiding (Left, Right)
import Lib
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

-- The content of a single slot, comparable regardless of whether it's a
-- corner, edge, or center - used to check both goals and protected regions
-- against the same kind of value.
data SlotValue
  = CornerValue (Map Vec3 Color)
  | EdgeValue (Map Vec3 Color) HexOrientation
  | CenterValue Color
  deriving (Eq, Show)

slotValue :: Vec3 -> BallState -> Maybe SlotValue
slotValue v st = case Map.lookup v (bsCorners st) of
  Just m -> Just (CornerValue m)
  Nothing -> case Map.lookup v (bsEdges st) of
    Just (m, o) -> Just (EdgeValue m o)
    Nothing -> CenterValue <$> Map.lookup v (bsCenters st)

allSlots :: Set Vec3
allSlots = Set.fromList (allCornerSlots ++ allEdgeSlots ++ map faceVec allFaceLocations)

-- Add the centers touching each given slot to the set. "This edge/corner is
-- in the right place" is only meaningful together with "and its neighboring
-- centers are too" - an edge/corner slot matching some reference is a hollow
-- win if the centers that give that slot its meaning were free to wander off
-- to a different position while the search wasn't looking. Almost every
-- protect/goal slot list for edges and corners should be built through this,
-- not passed raw - use the raw slot list directly only if you deliberately
-- want centers left unconstrained.
--
-- Note this still only protects centers at their ABSOLUTE starting slot. If
-- what you actually want is "correctly placed no matter how the whole ball
-- ended up oriented", use edgeHome/cornerHome instead, which are anchored to
-- color identity rather than fixed positions.
withTouchingCenters :: [Vec3] -> Set Vec3
withTouchingCenters slots =
  Set.fromList slots `Set.union` Set.fromList (concatMap (map faceVec . touchingFaces) slots)

-- A partial description of a desired ball state: the listed slots must match
-- the corresponding slots of `goalRef`. Slots not listed are unconstrained.
-- This is an ABSOLUTE description - "slot X must hold Y" - which is exactly
-- what you want when you're pinning down specific slots elsewhere too (e.g.
-- via withTouchingCenters), but not what you want if all you actually know
-- is "these two colors should be adjacent, wherever they currently are" -
-- for that, use edgeHome/cornerHome instead.
data Goal = Goal
  { goalSlots :: Set Vec3
  , goalRef   :: BallState
  } deriving (Eq, Show)

-- A goal matching the given reference ball (e.g. solvedGearBall, or some
-- other hand-built target) at the given slots.
goalFrom :: GearBall -> [Vec3] -> Goal
goalFrom ball slots = Goal (Set.fromList slots) (toBallState ball)

solvedGoal :: [Vec3] -> Goal
solvedGoal = goalFrom solvedGearBall

-- Does the given state match the goal's reference at the goal's slots?
matches :: Goal -> BallState -> Bool
matches (Goal slots ref) st = all (\v -> slotValue v st == slotValue v ref) (Set.toList slots)

-- Does `result` match `start` at every slot in the given set?
preserves :: Set Vec3 -> BallState -> BallState -> Bool
preserves protect start result = all (\v -> slotValue v result == slotValue v start) (Set.toList protect)

-- The slot currently holding the center with this color. Errors if the color
-- isn't present as a center in this state, which shouldn't happen for any
-- state reachable from solvedGearBall.
centerSlotOf :: Color -> BallState -> Vec3
centerSlotOf c st = case [v | (v, c') <- Map.toList (bsCenters st), c' == c] of
  [v] -> v
  xs  -> error ("centerSlotOf: expected exactly one " ++ show c ++ " center, found " ++ show (length xs))

sameTwo :: Color -> Color -> [Color] -> Bool
sameTwo c1 c2 vs = vs == [c1, c2] || vs == [c2, c1]

sameThree :: Color -> Color -> Color -> [Color] -> Bool
sameThree c1 c2 c3 vs = all (`elem` vs) [c1, c2, c3] && all (`elem` [c1, c2, c3]) vs

-- The slot currently holding the edge carrying exactly these two colors.
edgeWithColors :: Color -> Color -> BallState -> Vec3
edgeWithColors c1 c2 st =
  case [v | (v, (m, _)) <- Map.toList (bsEdges st), sameTwo c1 c2 (Map.elems m)] of
    [v] -> v
    xs  -> error ("edgeWithColors: expected exactly one match, found " ++ show (length xs))

-- The slot currently holding the corner carrying exactly these three colors.
cornerWithColors :: Color -> Color -> Color -> BallState -> Vec3
cornerWithColors c1 c2 c3 st =
  case [v | (v, m) <- Map.toList (bsCorners st), sameThree c1 c2 c3 (Map.elems m)] of
    [v] -> v
    xs  -> error ("cornerWithColors: expected exactly one match, found " ++ show (length xs))

-- Is the {c1,c2} edge sitting exactly between wherever the c1 and c2 centers
-- currently are, with c1 facing the c1 center and c2 facing the c2 center?
-- This is rotation-agnostic: it doesn't care WHERE those centers ended up,
-- only that the edge is correctly placed and wing-oriented relative to them.
-- It does NOT check hex-orientation - "home" here is about position and
-- wing, not gear rotation.
edgeHome :: Color -> Color -> BallState -> Bool
edgeHome c1 c2 st =
  let p1 = centerSlotOf c1 st
      p2 = centerSlotOf c2 st
      slot = p1 `addVec3` p2
  in case Map.lookup slot (bsEdges st) of
       Just (inner, _) -> Map.lookup p1 inner == Just c1 && Map.lookup p2 inner == Just c2
       Nothing -> False

-- As edgeHome, for a corner and its three neighboring centers.
cornerHome :: Color -> Color -> Color -> BallState -> Bool
cornerHome c1 c2 c3 st =
  let p1 = centerSlotOf c1 st
      p2 = centerSlotOf c2 st
      p3 = centerSlotOf c3 st
      slot = p1 `addVec3` p2 `addVec3` p3
  in case Map.lookup slot (bsCorners st) of
       Just inner ->
         Map.lookup p1 inner == Just c1 && Map.lookup p2 inner == Just c2 && Map.lookup p3 inner == Just c3
       Nothing -> False

-- An explicit correspondence between old and new facing directions for one
-- piece - e.g. [(Front,Front),(Right,Left)] for "FR moves to FL, with the
-- color facing Front staying on Front and the color facing Right now facing
-- Left." Two entries for an edge, three for a corner. This says exactly
-- what "FR to FL" colloquially means (as opposed to "FR to LF", a different
-- instruction) - it's the caller's job to state the correspondence, not
-- something to infer from which direction happens to be shared, since nothing
-- about the puzzle privileges that reading and it doesn't even make sense
-- when the two slots share no direction at all.
type Relabeling = [(FaceLocation, FaceLocation)]

fromSlotOf :: Relabeling -> Vec3
fromSlotOf = foldr1 addVec3 . map (faceVec . fst)

toSlotOf :: Relabeling -> Vec3
toSlotOf = foldr1 addVec3 . map (faceVec . snd)

-- Purely positional, color-blind check: does the edge now at the mapping's
-- target slot show exactly what the mapping says it should, given what was
-- at the source slot in `start`? Doesn't check hex-orientation - this is
-- about position and wing, not gear rotation.
edgeRelocated :: Relabeling -> BallState -> BallState -> Bool
edgeRelocated mapping start result =
  case (Map.lookup (fromSlotOf mapping) (bsEdges start), Map.lookup (toSlotOf mapping) (bsEdges result)) of
    (Just (fromInner, _), Just (toInner, _)) ->
      toInner == Map.fromList [(faceVec toF, fromInner Map.! faceVec fromF) | (fromF, toF) <- mapping]
    _ -> False

-- As edgeRelocated, for a corner (a three-entry Relabeling).
cornerRelocated :: Relabeling -> BallState -> BallState -> Bool
cornerRelocated mapping start result =
  case (Map.lookup (fromSlotOf mapping) (bsCorners start), Map.lookup (toSlotOf mapping) (bsCorners result)) of
    (Just fromInner, Just toInner) ->
      toInner == Map.fromList [(faceVec toF, fromInner Map.! faceVec fromF) | (fromF, toF) <- mapping]
    _ -> False

-- Iterative-deepening search for the shortest sequence of moves, drawn only
-- from `moves`, reaching a state satisfying `isSolution`. Tries every
-- sequence of length 0, then every sequence of length 1, etc., up to
-- `maxLen`, so the first solution found is shortest; returns Nothing if none
-- exists within `maxLen` moves.
--
-- `isSolution` is checked only against the state after the moves are
-- applied - build it out of matches/preserves/edgeHome/cornerHome (or your
-- own predicate) depending on whether "success" means matching an absolute
-- reference, staying fixed relative to how the search started, or being
-- correctly placed relative to wherever colors ended up.
--
-- A move is never immediately followed by its own inverse (that's always
-- wasted length), but otherwise this explores naively - no commutators yet.
search :: Int -> [Move] -> (BallState -> Bool) -> GearBall -> Maybe [Move]
search maxLen moves isSolution ball = firstJust [searchExact d | d <- [0 .. maxLen]]
  where
    start = toBallState ball

    -- DFS restricted to exactly `k` more moves; only checks `isSolution`
    -- once `k` reaches 0, so a call to `searchExact d` only ever returns
    -- sequences of length exactly `d`.
    searchExact d = go d [] start Nothing
      where
        go 0 acc st _
          | isSolution st = Just (reverse acc)
          | otherwise = Nothing
        go k acc st prevMove =
          firstJust
            [ go (k - 1) (m : acc) (applyBaseTurn f p st) (Just m)
            | m@(f, p) <- moves
            , not (isInverse prevMove m)
            ]

    isInverse Nothing _ = False
    isInverse (Just (f0, p0)) (f, p) = f0 == f && p0 /= p

    firstJust = foldr (\x r -> maybe r Just x) Nothing

-- Convenience for the common case: reach `goal` while preserving `protect`
-- exactly as `ball` started. Equivalent to calling `search` directly with
-- `\st -> matches goal st && preserves protect (toBallState ball) st`.
searchFor :: Int -> [Move] -> Set Vec3 -> Goal -> GearBall -> Maybe [Move]
searchFor maxLen moves protect goal ball =
  search maxLen moves (\st -> matches goal st && preserves protect (toBallState ball) st) ball
