module Lib
    ( -- * Static representation
      Color(..)
    , HexOrientation(..)
    , HexEdge(..)
    , Corner(..)
    , Center(..)
    , Face(..)
    , FaceLocation(..)
    , GearBall(..)
      -- * Solved ball
    , solvedGearBall
    , isSolved
    , isSolvedUpToReorientation
      -- * Turns
    , Turn(..)
    , parseTurn
    , parseMoves
    , applyTurn
    , applyMoves
    ) where

import Prelude hiding (Left, Right)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Char (isDigit)
import Data.List (foldl')
import Text.Read (readMaybe)

-- There are six valid colors for a gear ball.
data Color = Red
           | Green
           | Blue
           | Yellow
           | Purple
           | Orange
           deriving (Eq, Show)

-- The orientation of the central gear of a HexEdge. Zero is correctly oriented.
-- The other numbers represent the number of clockwise turns required to produce that orientation, starting from Zero.
--
-- This is tracked as a plain accumulator: it counts physical gear turns (from meshing during face
-- turns) and is not redefined relative to which color is currently listed first in a HexEdge.
data HexOrientation = Zero | One | Two | Three | Four | Five deriving (Eq, Show)

-- A HexEdge consists of a hexagonal gear with two colors. This gear can be rotated in 6 unique ways.
-- The first color is considered to be the "facing" color when the edge is referenced from a face.
data HexEdge = HexEdge Color Color HexOrientation deriving (Eq, Show)

-- A corner consists of three colors. The first color is considered to be the "facing" color when the corner is referenced from a face.
-- The second color is the next color clockwise from the first color.
data Corner = Corner Color Color Color deriving (Eq, Show)

-- A center consists of a single color.
data Center = Center Color deriving (Eq, Show)

-- A face consists of four corners, four edges, and a center.
-- The corners are ordered clockwise, as are the edges. The first edge refrenced comes directly after the first corner.
-- This ordering starts from the top left corner. For the "front" face, this position should be obvious. For the "left", "right", and "back" faces,
-- the top left corner is the one that would be the top left corner of the "front" face if the cube was rotated along its vertical axis, the axis perpendicular to the up and down faces.
-- For the "up" and "down" faces, the top left corner is the one that would be the top left corner of the "front" face if the cube was rotated along the axis perpendicular to the left and right faces.
data Face = Face FaceLocation Corner Corner Corner Corner HexEdge HexEdge HexEdge HexEdge Center deriving (Eq, Show)

data FaceLocation = Up
                  | Down
                  | Left
                  | Right
                  | Front
                  | Back
                  deriving (Eq, Show)

-- A gear ball consists of six faces.
data GearBall = GearBall Face Face Face Face Face Face deriving (Eq, Show)

--------------------------------------------------------------------------------
-- Geometry
--
-- Pieces are addressed internally by position in a fixed +/-1 coordinate frame:
-- Right=+x, Up=+y, Front=+z (and their opposites). A corner sits at a point with
-- all three coordinates +-1; an edge sits at a point with exactly one coordinate
-- 0; a center sits at one of the six unit vectors. This lets every face turn be
-- expressed as one of a handful of signed-axis rotations, applied uniformly to
-- both a piece's slot and the colors it carries, instead of hand-transcribing
-- the permutation for all six faces separately.
--------------------------------------------------------------------------------

type Vec3 = (Int, Int, Int)

faceVec :: FaceLocation -> Vec3
faceVec Up    = (0, 1, 0)
faceVec Down  = (0, -1, 0)
faceVec Left  = (-1, 0, 0)
faceVec Right = (1, 0, 0)
faceVec Front = (0, 0, 1)
faceVec Back  = (0, 0, -1)

-- Inverse of faceVec, defined only on the six unit vectors.
faceOf :: Vec3 -> FaceLocation
faceOf (0, 1, 0)  = Up
faceOf (0, -1, 0) = Down
faceOf (-1, 0, 0) = Left
faceOf (1, 0, 0)  = Right
faceOf (0, 0, 1)  = Front
faceOf (0, 0, -1) = Back
faceOf v = error ("faceOf: not a unit face vector: " ++ show v)

opposite :: FaceLocation -> FaceLocation
opposite Up    = Down
opposite Down  = Up
opposite Left  = Right
opposite Right = Left
opposite Front = Back
opposite Back  = Front

allFaceLocations :: [FaceLocation]
allFaceLocations = [Up, Down, Left, Right, Front, Back]

dot :: Vec3 -> Vec3 -> Int
dot (a, b, c) (d, e, f) = a * d + b * e + c * f

subVec :: Vec3 -> Vec3 -> Vec3
subVec (a, b, c) (d, e, f) = (a - d, b - e, c - f)

-- A 180-degree spin about the axis running through `face` and its opposite.
-- 180 degrees is its own inverse, so this doesn't depend on turn direction,
-- only on axis - it's the same formula for e.g. both Right and Left.
faceSpin180 :: FaceLocation -> Vec3 -> Vec3
faceSpin180 face = case face of
  Up    -> yAxis
  Down  -> yAxis
  Left  -> xAxis
  Right -> xAxis
  Front -> zAxis
  Back  -> zAxis
  where
    xAxis (x, y, z) = (x, -y, -z)
    yAxis (x, y, z) = (-x, y, -z)
    zAxis (x, y, z) = (-x, -y, z)

-- A 90-degree rotation of the slice parallel to `face`, clockwise as viewed
-- from `face`. E.g. slice90CW Right is the rotation the M slice undergoes
-- when R is turned.
slice90CW :: FaceLocation -> Vec3 -> Vec3
slice90CW face = case face of
  Right -> \(x, y, z) -> (x, z, -y)
  Left  -> \(x, y, z) -> (x, -z, y)
  Up    -> \(x, y, z) -> (-z, y, x)
  Down  -> \(x, y, z) -> (z, y, -x)
  Front -> \(x, y, z) -> (y, -x, z)
  Back  -> \(x, y, z) -> (-y, x, z)

-- The clockwise (viewed from the given corner slot's first-listed face, below)
-- cyclic order of the three directions meeting at a corner. This is the one
-- piece of geometry that can't be derived from symmetry alone - it fixes which
-- of the two non-addressing faces is "next clockwise".
cornerTriple :: Vec3 -> [Vec3]
cornerTriple (-1, 1, 1)  = [(0, 0, 1), (0, 1, 0), (-1, 0, 0)]  -- ULF: F,U,L
cornerTriple (1, 1, 1)   = [(0, 0, 1), (1, 0, 0), (0, 1, 0)]   -- URF: F,R,U
cornerTriple (1, -1, 1)  = [(0, 0, 1), (0, -1, 0), (1, 0, 0)]  -- DFR: F,D,R
cornerTriple (-1, -1, 1) = [(0, 0, 1), (-1, 0, 0), (0, -1, 0)] -- DLF: F,L,D
cornerTriple (1, 1, -1)  = [(1, 0, 0), (0, 0, -1), (0, 1, 0)]  -- UBR: R,B,U
cornerTriple (1, -1, -1) = [(1, 0, 0), (0, -1, 0), (0, 0, -1)] -- DBR: R,D,B
cornerTriple (-1, 1, -1) = [(-1, 0, 0), (0, 1, 0), (0, 0, -1)] -- ULB: L,U,B
cornerTriple (-1, -1, -1) = [(-1, 0, 0), (0, 0, -1), (0, -1, 0)] -- DLB: L,B,D
cornerTriple v = error ("cornerTriple: not a corner slot: " ++ show v)

-- Rotate a cyclic list so that it starts at the given element.
rotateTo :: Eq a => a -> [a] -> [a]
rotateTo x xs = let (pre, post) = break (== x) xs in post ++ pre

allCornerSlots :: [Vec3]
allCornerSlots = [(x, y, z) | x <- [-1, 1], y <- [-1, 1], z <- [-1, 1]]

allEdgeSlots :: [Vec3]
allEdgeSlots =
  [(0, y, z) | y <- [-1, 1], z <- [-1, 1]] ++
  [(x, 0, z) | x <- [-1, 1], z <- [-1, 1]] ++
  [(x, y, 0) | x <- [-1, 1], y <- [-1, 1]]

-- The (up to two) faces an edge slot touches.
touchingFaces :: Vec3 -> [FaceLocation]
touchingFaces (x, y, z) = [faceOf v | v <- [(x, 0, 0), (0, y, 0), (0, 0, z)], v /= (0, 0, 0)]

-- Each face's own four corner slots and four edge slots, clockwise from the
-- top-left, per the Face documentation. Cross-derived and validated against
-- each other (every corner/edge appears identically from every face it touches).
faceCorners :: FaceLocation -> [Vec3]
faceCorners Front = [(-1, 1, 1), (1, 1, 1), (1, -1, 1), (-1, -1, 1)]
faceCorners Right = [(1, 1, 1), (1, 1, -1), (1, -1, -1), (1, -1, 1)]
faceCorners Back  = [(1, 1, -1), (-1, 1, -1), (-1, -1, -1), (1, -1, -1)]
faceCorners Left  = [(-1, 1, -1), (-1, 1, 1), (-1, -1, 1), (-1, -1, -1)]
faceCorners Up    = [(-1, 1, -1), (1, 1, -1), (1, 1, 1), (-1, 1, 1)]
faceCorners Down  = [(-1, -1, 1), (1, -1, 1), (1, -1, -1), (-1, -1, -1)]

faceEdges :: FaceLocation -> [Vec3]
faceEdges Front = [(0, 1, 1), (1, 0, 1), (0, -1, 1), (-1, 0, 1)]
faceEdges Right = [(1, 1, 0), (1, 0, -1), (1, -1, 0), (1, 0, 1)]
faceEdges Back  = [(0, 1, -1), (-1, 0, -1), (0, -1, -1), (1, 0, -1)]
faceEdges Left  = [(-1, 1, 0), (-1, 0, 1), (-1, -1, 0), (-1, 0, -1)]
faceEdges Up    = [(0, 1, -1), (1, 1, 0), (0, 1, 1), (-1, 1, 0)]
faceEdges Down  = [(0, -1, 1), (1, -1, 0), (0, -1, -1), (-1, -1, 0)]

--------------------------------------------------------------------------------
-- HexOrientation arithmetic
--------------------------------------------------------------------------------

orientToInt :: HexOrientation -> Int
orientToInt Zero  = 0
orientToInt One   = 1
orientToInt Two   = 2
orientToInt Three = 3
orientToInt Four  = 4
orientToInt Five  = 5

intToOrient :: Int -> HexOrientation
intToOrient n = [Zero, One, Two, Three, Four, Five] !! (n `mod` 6)

addOrient :: HexOrientation -> Int -> HexOrientation
addOrient o delta = intToOrient (orientToInt o + delta)

--------------------------------------------------------------------------------
-- Canonical (position-indexed) ball state, used to compute turns
--------------------------------------------------------------------------------

data BallState = BallState
  { bsCorners :: Map Vec3 (Map Vec3 Color)
  , bsEdges   :: Map Vec3 (Map Vec3 Color, HexOrientation)
  , bsCenters :: Map Vec3 Color
  } deriving (Eq, Show)

toBallState :: GearBall -> BallState
toBallState (GearBall f1 f2 f3 f4 f5 f6) = BallState
  { bsCorners = Map.fromList (concatMap cornersOf faces)
  , bsEdges   = Map.fromList (concatMap edgesOf faces)
  , bsCenters = Map.fromList (map centerOf faces)
  }
  where
    faces = [f1, f2, f3, f4, f5, f6]

    cornersOf :: Face -> [(Vec3, Map Vec3 Color)]
    cornersOf (Face loc c0 c1 c2 c3 _ _ _ _ _) = zipWith toEntry (faceCorners loc) [c0, c1, c2, c3]
      where
        toEntry slot (Corner a b c) = case rotateTo (faceVec loc) (cornerTriple slot) of
          [d0, d1, d2] -> (slot, Map.fromList [(d0, a), (d1, b), (d2, c)])
          _            -> error "cornersOf: corner slot must have exactly three directions"

    edgesOf :: Face -> [(Vec3, (Map Vec3 Color, HexOrientation))]
    edgesOf (Face loc _ _ _ _ e0 e1 e2 e3 _) = zipWith toEntry (faceEdges loc) [e0, e1, e2, e3]
      where
        toEntry slot (HexEdge a b orient) =
          (slot, (Map.fromList [(faceVec loc, a), (subVec slot (faceVec loc), b)], orient))

    centerOf :: Face -> (Vec3, Color)
    centerOf (Face loc _ _ _ _ _ _ _ _ (Center col)) = (faceVec loc, col)

fromBallState :: BallState -> GearBall
fromBallState st = case map buildFace allFaceLocations of
  [fUp, fDown, fLeft, fRight, fFront, fBack] -> GearBall fUp fDown fLeft fRight fFront fBack
  _ -> error "fromBallState: expected exactly six faces"
  where
    buildFace loc = case (map buildCorner (faceCorners loc), map buildEdge (faceEdges loc)) of
      ([c0, c1, c2, c3], [e0, e1, e2, e3]) -> Face loc c0 c1 c2 c3 e0 e1 e2 e3 center
      _ -> error "buildFace: expected exactly four corners and four edges"
      where
        center = Center (bsCenters st Map.! faceVec loc)

        buildCorner slot = case map (inner Map.!) (rotateTo (faceVec loc) (cornerTriple slot)) of
          [a, b, c] -> Corner a b c
          _         -> error "buildCorner: corner slot must have exactly three directions"
          where inner = bsCorners st Map.! slot

        buildEdge slot = HexEdge (inner Map.! faceVec loc) (inner Map.! subVec slot (faceVec loc)) orient
          where (inner, orient) = bsEdges st Map.! slot

--------------------------------------------------------------------------------
-- Solved ball
--------------------------------------------------------------------------------

-- An arbitrary but fixed assignment of colors to faces.
colorFor :: FaceLocation -> Color
colorFor Up    = Yellow
colorFor Down  = Orange
colorFor Left  = Purple
colorFor Right = Red
colorFor Front = Blue
colorFor Back  = Green

solvedGearBall :: GearBall
solvedGearBall = fromBallState BallState
  { bsCorners = Map.fromList
      [ (slot, Map.fromList [(d, colorFor (faceOf d)) | d <- cornerTriple slot])
      | slot <- allCornerSlots
      ]
  , bsEdges = Map.fromList
      [ (slot, (Map.fromList [(faceVec f, colorFor f) | f <- touchingFaces slot], Zero))
      | slot <- allEdgeSlots
      ]
  , bsCenters = Map.fromList [(faceVec loc, colorFor loc) | loc <- allFaceLocations]
  }

isSolved :: GearBall -> Bool
isSolved = (== solvedGearBall)

-- True when every face shows a single color - i.e. the ball is solved, or is
-- solved but reoriented as a whole (e.g. after "R L'"). A pure reorientation
-- can't scramble anything, only relabel which face is "up", so this is a
-- weaker, rotation-agnostic notion of solved.
isSolvedUpToReorientation :: GearBall -> Bool
isSolvedUpToReorientation (GearBall f1 f2 f3 f4 f5 f6) = all isFaceUniform [f1, f2, f3, f4, f5, f6]
  where
    isFaceUniform (Face _ c0 c1 c2 c3 e0 e1 e2 e3 (Center col)) = case
      [facingCorner c0, facingCorner c1, facingCorner c2, facingCorner c3
      ,facingEdge e0, facingEdge e1, facingEdge e2, facingEdge e3, col] of
        (c : cs) -> all (== c) cs
        []       -> True
      where
        facingCorner (Corner a _ _) = a
        facingEdge (HexEdge a _ _) = a

--------------------------------------------------------------------------------
-- Turns
--------------------------------------------------------------------------------

-- A turn of a face by 180 degrees. `Bool` is True for the prime (counter-
-- clockwise) direction. `Int` is the number of times to repeat it (1-6).
-- There is no notion of a 90-degree turn: turning any face 90 degrees leaves
-- the gearing misaligned, so 180 degrees is the smallest legal move.
data Turn = Turn FaceLocation Bool Int deriving (Eq, Show)

-- Turning face `face` clockwise 180 degrees:
--  - relocates that face's own 4 corners and 4 edges 180 degrees around,
--    unchanged as addressed from `face` (its gears don't turn).
--  - drags the parallel slice 90 degrees clockwise (as viewed from `face`),
--    cycling its 4 centers and 4 edges, and advancing each of those edges'
--    gears by one clockwise notch.
--  - leaves the opposite face's layer untouched.
-- Turning `face` counter-clockwise (prime) does the same to `face`'s own
-- layer (180 degrees is its own inverse), but drags the slice the other way,
-- using the opposite face's clockwise rotation, and the gears turn back a
-- notch instead.
applyBaseTurn :: FaceLocation -> Bool -> BallState -> BallState
applyBaseTurn face isPrime (BallState cs es ctrs) = BallState
  { bsCorners = Map.fromList (map moveCorner (Map.toList cs))
  , bsEdges   = Map.fromList (map moveEdge (Map.toList es))
  , bsCenters = Map.fromList (map moveCenter (Map.toList ctrs))
  }
  where
    axis = faceVec face
    fSpin = faceSpin180 face
    sSpin = slice90CW (if isPrime then opposite face else face)
    gearDelta = if isPrime then -1 else 1

    layerOf pos = dot pos axis

    moveCorner (pos, inner) = case layerOf pos of
      1 -> (fSpin pos, Map.mapKeys fSpin inner)
      _ -> (pos, inner)

    moveEdge (pos, (inner, orient)) = case layerOf pos of
      1 -> (fSpin pos, (Map.mapKeys fSpin inner, orient))
      0 -> (sSpin pos, (Map.mapKeys sSpin inner, addOrient orient gearDelta))
      _ -> (pos, (inner, orient))

    moveCenter (pos, col) = case layerOf pos of
      0 -> (sSpin pos, col)
      _ -> (pos, col)

applyTurn :: GearBall -> Turn -> GearBall
applyTurn ball (Turn face isPrime n) =
  fromBallState (iterate (applyBaseTurn face isPrime) (toBallState ball) !! n)

applyMoves :: GearBall -> [Turn] -> GearBall
applyMoves = foldl' applyTurn

--------------------------------------------------------------------------------
-- Notation
--------------------------------------------------------------------------------

-- Parses standard notation: one of RLFBUD, optionally followed by ' for
-- counter-clockwise, optionally followed by a repeat count from 2 to 6
-- (a bare letter means a single turn). E.g. "R", "R'", "R2", "F'5".
parseTurn :: String -> Maybe Turn
parseTurn [] = Nothing
parseTurn (c : rest) = do
  face <- faceFromChar c
  let (isPrime, afterPrime) = case rest of
        ('\'' : xs) -> (True, xs)
        xs          -> (False, xs)
  n <- case afterPrime of
    []                          -> Just 1
    digits | all isDigit digits -> readMaybe digits >>= \v -> if v >= 2 && v <= 6 then Just v else Nothing
           | otherwise          -> Nothing
  Just (Turn face isPrime n)
  where
    faceFromChar 'R' = Just Right
    faceFromChar 'L' = Just Left
    faceFromChar 'U' = Just Up
    faceFromChar 'D' = Just Down
    faceFromChar 'F' = Just Front
    faceFromChar 'B' = Just Back
    faceFromChar _   = Nothing

-- Parses a whitespace-separated sequence of moves, e.g. "R U F' D2".
parseMoves :: String -> Maybe [Turn]
parseMoves = traverse parseTurn . words
