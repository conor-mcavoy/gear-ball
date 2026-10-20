# GearBall

A Haskell model and solving toolkit for a "Gear Ball" — a spherical Rubik's-cube-like
puzzle where every edge position is a hexagonal gear that meshes with its neighbors.
Goal of the project: model the puzzle precisely, then use search to discover
building-phase techniques and algorithms toward a full solving method.

## Physical mechanics (established facts)

- **Every legal turn is 180°.** A 90° turn leaves the gearing misaligned and isn't a
  valid state. So "R" in our notation always means what classic-style notation would call
  R2 — there's no "single quarter turn" primitive.
- **A turn does two things at once.** Turning face X 180° (clockwise, viewed from X):
  - X's own layer (its 4 corners + 4 edges) relocates 180° within itself, as expected.
  - The parallel slice (the 4 centers + 4 edges not touching X or its opposite) gets
    dragged 90° clockwise (as viewed from X) by gear meshing, and each of those slice
    edges' hex-gear advances one notch clockwise (`One`). The opposite face's layer
    (X's antipode) is untouched.
  - Counter-clockwise (X') does the same 180° to X's own layer (180° is its own
    inverse, so direction doesn't matter there), but drags the slice the *other* way
    and the gear notch goes the other way (`Five`, i.e. -1 mod 6).
- **A single move has order 12.** 12 consecutive same-face turns return to solved.
- **Opposite-face turns are equivalent up to reorientation.** `R L'` is a pure
  whole-ball rotation.
- **Corners can only ever be permuted, never twisted**, by any sequence of moves
  whatsoever. Proof sketch: every operation touching a corner is a 180° rotation about
  one of the 3 cube axes, so this is like the R2L2U2D2F2B2 state of a 3x3.
- **Edges are heavily paired/constrained**, much more than a standard 3x3 (this
  puzzle's 180-only move set is structurally similar to the "double-move-only" 3x3
  variant). For example: two edges sharing one color, whose other two colors are opposite
  each other (e.g. Red-Yellow and Purple-Yellow, since Red/Purple are opposite), are
  always either cube-opposite (antipodal slots) or face-opposite with the shared
  color's center adjacent to *both* or *neither*. There are no other possible positions.
  Hex-orientations of such edges also always match exactly. This is why large chunks of the ball "fall
  into place" once a few pieces are positioned correctly during manual solving.

## Source layout

- **`src/Lib.hs`** — the static representation and the turn engine.
- **`src/Search.hs`** — the constrained-search toolkit built on top of `Lib`.
- **`src/Ergonomics.hs`** — scores how pleasant an algorithm is to *perform*.
- **`app/Main.hs`** — minimal demo (not load-bearing).
- **`test/Spec.hs`** — plain-assertion test suite (no framework — see Build notes).

## Static representation (`Lib.hs`)

Straightforward ADTs: `Color` (6 values), `HexOrientation` (`Zero`..`Five`, a plain
mod-6 accumulator of gear turns),
`HexEdge Color Color HexOrientation`, `Corner Color Color Color`, `Center
Color`, `Face FaceLocation Corner×4 HexEdge×4 Center`, `GearBall Face×6`. Convention
throughout: the first-listed color is whichever one faces the addressing face; for
corners the second color is the next one clockwise.

`FaceLocation` is `Up | Down | Left | Right | Front | Back`.

`solvedGearBall` uses the fixed assignment Up=Yellow, Down=Orange, Left=Purple,
Right=Red, Front=Blue, Back=Green.

## Geometry engine (internals of `Lib.hs`)

Pieces are addressed by position in a fixed ±1 coordinate frame (`type Vec3 =
(Int,Int,Int)`, Right=+x/Up=+y/Front=+z and opposites). A corner sits at an all-±1
point, an edge at a point with exactly one 0, a center at a unit vector. This lets
every one of the 6 possible face turns share the same rotation math (`faceSpin180`
for the 180° own-layer part, `slice90CW` for the 90° slice-drag part).

`BallState` is the internal position-indexed representation (`Map Vec3 (Map Vec3
Color)` for corners, similar-with-orientation for edges, `Map Vec3 Color` for
centers) — used for all move computation. `toBallState`/`fromBallState` convert
to/from the public `GearBall`.

`reorientBall`/`reorientBallState` pick the whole ball up and re-hold it 90° from a
given face.

## Notation (`Lib.hs`)

Standard `RLFBUD` letters, optional `'` for counter-clockwise, optional repeat count
2–6 (bare letter = 1 turn). `parseTurn`/`parseMoves` parse it; `Turn FaceLocation Bool
Int` is the notation-level type; `applyTurn`/`applyMoves` apply it to a `GearBall`.
For search purposes there's a simpler primitive-move type, `type Move = (FaceLocation,
Bool)` (always exactly one 180° turn, no count) — `allMoves` (all 12),
`movesFor [faces]` (restrict the alphabet), `renderMove`/`renderMoves` for
pretty-printing search results back to notation.

## Search toolkit (`Search.hs`)

The core primitive:

```haskell
search :: Int -> [Move] -> (BallState -> Bool) -> GearBall -> Maybe [Move]
```

Iterative-deepening search (tries all length-0 sequences, then length-1, etc., up to
`maxLen`) for the *shortest* sequence, reaching a state satisfying an arbitrary predicate.
No commutator support yet, never really needed it.

The predicate is where all the design care lives:

**"Is this piece correctly placed relative to its own colors, wherever those colors
currently are?"** — rotation-agnostic, useful for verifying something is correct
independent of how the whole ball happens to be oriented.
- `centerSlotOf color state` — where a given color's center currently sits.
- `edgeWithColors c1 c2 state` / `cornerWithColors c1 c2 c3 state` — locate a piece by
  its color identity.
- `edgeHome c1 c2 state` / `cornerHome c1 c2 c3 state` — is this piece sitting exactly
  between wherever its own colors' centers currently are, correctly wing-oriented?
  (Position + wing only, not hex-orientation.)

**"Does the piece at slot A end up at slot B, in this specific orientation?"** — pure
position, *zero color references*, matches what "swap FR and FL" actually,
colloquially means. This is the right tool whenever the request is about moving
pieces around rather than about a specific color ending up somewhere.
- `type Relabeling = [(FaceLocation, FaceLocation)]` — an explicit correspondence
  between old and new facing directions, e.g. `[(Front,Front),(Right,Left)]` for "FR
  moves to FL, F stays F, R becomes L." The correspondence must be stated explicitly
  by the caller (2 entries for an edge, 3 for a corner) — it is *not* inferred from
  whichever direction two slots happen to share, since that doesn't generalize (slots
  can share zero directions) and silently forecloses the "FR to LF" (flipped) reading.
- `edgeRelocated mapping start result` / `cornerRelocated mapping start result` —
  check a `Relabeling` holds between two states.

Absolute/reference-based tools (for "this must match a specific known state," e.g.
solved):
- `Goal { goalSlots :: Set Vec3, goalRef :: BallState }`, `goalFrom ball slots`,
  `solvedGoal slots`, `matches goal state`.
- `preserves protectSlots start result` — do the given slots hold the exact same
  value in `result` as they did in `start`.
- `withTouchingCenters slots` — expands a slot list to also include the centers each
  slot touches. **Almost always use this** when building a protect/goal slot list for
  edges/corners against an absolute reference — "this edge is in the right place" is a
  hollow claim if the centers that give the slot its meaning were left free to wander.
- `searchFor maxLen moves protectSlots goal ball` — convenience wrapper for the common
  absolute protect+goal case, built on top of `search`.
- `allSlots` — every corner/edge/center slot, useful with `Set.difference` for "protect
  everything except the target."

## Ergonomics (`Ergonomics.hs`)

Scores an algorithm by how comfortable it is to perform, not how short it is.

The physical model (from handling the real ball):

- **Face costs** — `R`/`L` = 1 (your hands are already there), `U` = 1.5,
  `D`/`F`/`B` = 2 (awkward regrip). Turning the *same face twice in a row* costs 2
  for the repeat regardless of face — the hand has to fully reset. So `R2` costs 3, for example.
- **The two-handed re-hold.** An `R` or `L` turn can be done with each hand turning
  90° instead of one hand turning 180°. The gear-drag on the middle slice exactly
  cancels the whole-ball rotation, so *the centers stay fixed in space* while the
  ball ends up re-held 90° round. Free, and it can turn a later awkward face into a
  `U`. The four pairings are `R`≡`R x'`, `R'`≡`R' x`, `L`≡`L x`, `L'`≡`L' x'`. 
  Whenever possible, we can substitute these moves to cause later moves to be more
  ergonomic. E.g. `R' F` can be `R' x U`, costing 1 + 1.5 rather than 1 + 2.

`score` gives the cheapest execution in the hold as written; `bestHold` minimises
over all 24 starting holds, which is only legitimate when the target is specified up
to whole-ball rotation (re-holding first means the algorithm performs a *rotated*
version of its operation). `renderExecution` prints the result with the free
re-holds shown as `x`/`x'`; `verifyExecution` re-runs an execution through the
engine and checks it matches the original algorithm up to the leftover rotation.

See `ALGORITHMS.md` for the best algorithms found so far.
See `METHOD.md` for general explanation of how to solve the ball.

## Build notes

- `stack build` / `stack test`. `stack.yaml` uses `resolver: ghc-9.6.7` with no
  curated snapshot — only GHC boot packages (`base`, `containers`) are reliably
  available without hitting Hackage, which is why the test suite is plain
  `IO`-and-assertions rather than hspec.
- `package.yaml` is the source of truth; stack auto-regenerates `gear-ball.cabal` from
  it on build (has its own embedded hpack even though no standalone `hpack` binary is
  installed) — don't hand-edit the `.cabal` file, and don't bother listing modules
  explicitly in `package.yaml`, they're auto-discovered from `src/`.
s