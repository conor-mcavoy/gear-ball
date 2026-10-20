-- | Scoring algorithms by how pleasant they are to actually perform, as
-- opposed to how short they are. This module never searches for anything - it
-- ranks sequences that `Search` has already produced, and the two concerns are
-- kept apart on purpose (see `Search.searchAll`).
--
-- The model comes from three observations about holding a real gear ball:
--
--   1. Turning a face needs a firm grip on that face /and/ on the opposite
--      one. R and L are easy, U is middling, and F, B and D are awkward.
--   2. An R or L turn can be performed two-handed - each hand turning 90
--      degrees rather than one hand turning 180 - which leaves the centers
--      fixed in space and leaves the ball re-held 90 degrees round. That
--      re-hold is free, and it can turn a later awkward face into an easy one.
--   3. Turning the same face twice in a row needs a full hand reset, so the
--      repeat is awkward regardless of which face it is.
module Ergonomics
    ( -- * The cost model
      Cost
    , baseCost
    , repeatCost
      -- * How the ball is held
    , Hold
    , identityHold
    , holdFace
    , rotateHold
    , allHolds
    , renderHold
      -- * Executions
    , Step(..)
    , Execution(..)
    , reholdFor
    , executions
    , scoreIn
    , score
    , bestHold
    , renderExecution
      -- * Checking the model against the engine
    , runExecution
    , verifyExecution
    ) where

import Prelude hiding (Left, Right)
import Lib
import Data.List (foldl', minimumBy)
import Data.Ord (comparing)
import qualified Data.Map.Strict as Map

-- | Lower is better. Costs are sums of 1, 1.5 and 2, all exactly
-- representable, so `Double` comparisons here are exact.
type Cost = Double

-- | What it costs to turn whichever face is currently in this world position.
-- R and L are what your hands are already gripping; U is reachable; D, F and B
-- all need an awkward regrip.
baseCost :: FaceLocation -> Cost
baseCost Right = 1
baseCost Left  = 1
baseCost Up    = 1.5
baseCost Down  = 2
baseCost Front = 2
baseCost Back  = 2

-- | Any turn of the same face as the one immediately before it. The hand has
-- to reset, which costs the same however easy that face was the first time.
repeatCost :: Cost
repeatCost = 2

-- | How the ball is currently held: for each of the ball's own faces, the
-- world position it currently occupies. `holdFace hold f` is therefore the
-- letter you write for a turn of the ball's @f@ face while holding it this
-- way.
newtype Hold = Hold [(FaceLocation, FaceLocation)] deriving (Eq, Show)

identityHold :: Hold
identityHold = Hold [(f, f) | f <- allFaceLocations]

holdFace :: Hold -> FaceLocation -> FaceLocation
holdFace (Hold m) f = case lookup f m of
  Just g -> g
  Nothing -> error ("holdFace: no entry for " ++ show f)

-- The face relabelling a whole-ball reorientation induces, read out of Lib's
-- own `reorientBallState` by watching where the center colors land. Deriving
-- it rather than hand-writing a table keeps it from drifting away from the
-- engine's idea of which way round a reorientation goes.
relabelFrom :: FaceLocation -> Bool -> [(FaceLocation, FaceLocation)]
relabelFrom axis isPrime =
  [ (orig, dest)
  | orig <- allFaceLocations
  , let col = centerAt solved orig
  , dest <- take 1 [ g | g <- allFaceLocations, centerAt rotated g == col ]
  ]
  where
    solved = toBallState solvedGearBall
    rotated = reorientBallState axis isPrime solved
    centerAt st f = case Map.lookup (faceVec f) (bsCenters st) of
      Just c -> c
      Nothing -> error "relabelFrom: missing center"

-- | Re-hold the ball 90 degrees about `axis` (counter-clockwise if the flag is
-- set), updating where every face has ended up.
rotateHold :: FaceLocation -> Bool -> Hold -> Hold
rotateHold axis isPrime (Hold m) = Hold [ (f, relabel g) | (f, g) <- m ]
  where
    table = relabelFrom axis isPrime
    relabel g = case lookup g table of
      Just h -> h
      Nothing -> error "rotateHold: incomplete relabelling"

-- | All 24 ways of picking the ball up. Only relevant when the algorithm's
-- target is specified up to whole-ball rotation - see `bestHold`.
allHolds :: [Hold]
allHolds = grow [identityHold] [identityHold]
  where
    gens = [ (f, p) | f <- allFaceLocations, p <- [False, True] ]
    grow seen [] = seen
    grow seen frontier =
      let new = [ h | x <- frontier, (f, p) <- gens
                , let h = rotateHold f p x, h `notElem` seen ]
          uniq = dedupe new
      in if null uniq then seen else grow (seen ++ uniq) uniq
    dedupe = foldl' (\acc x -> if x `elem` acc then acc else acc ++ [x]) []

-- | Which ball-face sits at each world position, e.g. @U=F D=B L=L R=R F=D B=U@.
renderHold :: Hold -> String
renderHold (Hold m) = unwords
  [ show' w ++ "=" ++ show' f
  | w <- allFaceLocations
  , (f, _) <- take 1 [ e | e@(_, g) <- m, g == w ]
  ]
  where show' = take 1 . show

-- | One move of an algorithm as actually performed: the letter you turn (in
-- the hold in force at that moment) and whether you did it two-handed, which
-- re-holds the ball as a side effect.
data Step = Step
  { stepMove   :: Move
  , stepRehold :: Bool
  } deriving (Eq, Show)

data Execution = Execution
  { exSteps :: [Step]
  , exCost  :: Cost
  , exHold  :: Hold   -- ^ how the ball is left at the end
  } deriving (Eq, Show)

-- | The re-hold that comes free with a two-handed R or L turn, as the
-- `isPrime` argument to a reorientation about world Right. Verified against
-- the engine: exactly these four pairings leave all six centers fixed in
-- space, and the two wrong pairings do not.
--
-- Note what this implies. The trick always spins the ball about the axis your
-- hands are already on - world R-L - so it can cycle U, F, D and B into each
-- other but can never move a face onto R or L, or off them. Choosing which
-- axis the algorithm works about is a separate decision, made by picking the
-- starting hold (`bestHold`), not by this.
reholdFor :: Move -> Maybe Bool
reholdFor (Right, False) = Just True    -- R  performed as  R x'
reholdFor (Right, True)  = Just False   -- R' performed as  R' x
reholdFor (Left,  False) = Just False   -- L  performed as  L x
reholdFor (Left,  True)  = Just True    -- L' performed as  L' x'
reholdFor _              = Nothing

-- | Every way of performing this algorithm from the given starting hold,
-- differing in which R/L turns are taken two-handed. Branching is at most
-- 2^(number of R/L turns).
executions :: Hold -> [Move] -> [Execution]
executions hold0 = go hold0 Nothing
  where
    go hold _ [] = [Execution [] 0 hold]
    go hold prev ((f, p) : rest) =
      let written = holdFace hold f
          c = if prev == Just written then repeatCost else baseCost written
          branch reheld hold' =
            [ Execution (Step (written, p) reheld : steps) (c + tot) fin
            | Execution steps tot fin <- go hold' (Just written) rest ]
      in branch False hold
         ++ case reholdFor (written, p) of
              Nothing -> []
              Just rp -> branch True (rotateHold Right rp hold)

-- | The cheapest way to perform the algorithm from a given hold. Ties are
-- broken towards fewer re-holds, since a re-hold that buys nothing is just
-- another thing to remember.
scoreIn :: Hold -> [Move] -> Execution
scoreIn hold = minimumBy (comparing key) . executions hold
  where key ex = (exCost ex, length (filter stepRehold (exSteps ex)))

-- | `scoreIn` from the ball held the way the algorithm is written.
score :: [Move] -> Execution
score = scoreIn identityHold

-- | The cheapest execution over all 24 starting holds.
--
-- Only valid when the thing you want is specified up to whole-ball rotation -
-- re-holding at the start means the algorithm performs a /rotated version/ of
-- its operation. "Swap the two edge pairs either side of some axis" is a
-- request this applies to; "swap FU with BU specifically" is not.
bestHold :: [Move] -> (Hold, Execution)
bestHold ms = minimumBy (comparing (exCost . snd)) [ (h, scoreIn h ms) | h <- allHolds ]

-- | An execution written out, with the free re-holds shown as @x@ / @x'@.
renderExecution :: Execution -> String
renderExecution = unwords . concatMap render . exSteps
  where
    render (Step m reheld) =
      renderMove m : [ if rp then "x'" else "x" | reheld, Just rp <- [reholdFor m] ]

-- | Perform an execution literally: each written letter turns the world face
-- with that name, and each re-hold physically reorients the ball.
runExecution :: Execution -> BallState -> BallState
runExecution ex st0 = foldl' step st0 (exSteps ex)
  where
    step st (Step m@(f, p) reheld) =
      let st' = applyBaseTurn f p st
      in case (reheld, reholdFor m) of
           (True, Just rp) -> reorientBallState Right rp st'
           _ -> st'

-- | Does this execution really do what the original move list does, up to the
-- whole-ball rotation it deliberately leaves behind?
--
-- Every re-hold is about world Right, so they all commute and the net leftover
-- rotation is just a power of x - which is why the check can compare against
-- the plainly-executed algorithm spun back by that power.
verifyExecution :: [Move] -> Execution -> BallState -> Bool
verifyExecution ms ex st = runExecution ex st == rotated
  where
    plain = foldl' (\s (f, p) -> applyBaseTurn f p s) st ms
    reholds = [ rp | Step m True <- exSteps ex, Just rp <- [reholdFor m] ]
    net = length (filter not reholds) - length (filter id reholds)
    rotated = iterate (reorientBallState Right False) plain !! (net `mod` 4)
