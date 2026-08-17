module Search
    ( SlotValue(..)
    , slotValue
    , Goal(..)
    , goalFrom
    , solvedGoal
    , allSlots
    , search
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

-- A partial description of a desired ball state: the listed slots must match
-- the corresponding slots of `goalRef`. Slots not listed are unconstrained.
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

matchesGoal :: Goal -> BallState -> Bool
matchesGoal (Goal slots ref) st = all (\v -> slotValue v st == slotValue v ref) (Set.toList slots)

preserves :: Set Vec3 -> BallState -> BallState -> Bool
preserves protect start result = all (\v -> slotValue v result == slotValue v start) (Set.toList protect)

-- Iterative-deepening search for the shortest sequence of moves, drawn only
-- from `moves`, that reaches `goal` while leaving every slot in `protect`
-- exactly as it started (position and orientation) - anything not in
-- `protect` is free to be disrupted. Tries every sequence of length 0, then
-- every sequence of length 1, etc., up to `maxLen`, so the first solution
-- found is shortest; returns Nothing if none exists within `maxLen` moves.
--
-- A move is never immediately followed by its own inverse (that's always
-- wasted length), but otherwise this explores naively - no commutators yet.
search :: Int -> [Move] -> Set Vec3 -> Goal -> GearBall -> Maybe [Move]
search maxLen moves protect goal ball = firstJust [searchExact d | d <- [0 .. maxLen]]
  where
    start = toBallState ball

    -- DFS restricted to exactly `k` more moves; only checks the goal once
    -- `k` reaches 0, so a call to `searchExact d` only ever returns
    -- sequences of length exactly `d`.
    searchExact d = go d [] start Nothing
      where
        go 0 acc st _
          | matchesGoal goal st && preserves protect start st = Just (reverse acc)
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
