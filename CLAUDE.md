# GearBall

A Haskell model and solving toolkit for a "Gear Ball" — a spherical Rubik's-cube-like
puzzle where every edge position is a hexagonal gear that meshes with its neighbors.
Goal of the project: model the puzzle precisely, then use search to discover
building-phase techniques and algorithms toward a full solving method.

## Physical mechanics (established facts)

- **Every legal turn is 180°.** A 90° turn leaves the gearing misaligned and isn't a
  valid state. So "R" in our notation always means what old-style notation would call
  R2 — there's no separate "single quarter turn" primitive.
- **A turn does two things at once.** Turning face X 180° (clockwise, viewed from X):
  - X's own layer (its 4 corners + 4 edges) relocates 180° within itself, *unchanged*
    as addressed from X — corner/edge color arrangement doesn't change, gears don't
    turn.
  - The parallel slice (the 4 centers + 4 edges not touching X or its opposite) gets
    dragged 90° clockwise (as viewed from X) by gear meshing, and each of those slice
    edges' hex-gear advances one notch clockwise (`One`). The opposite face's layer
    (X's antipode) is untouched.
  - Counter-clockwise (X') does the same 180° to X's own layer (180° is its own
    inverse, so direction doesn't matter there), but drags the slice the *other* way
    and the gear notch goes the other way (`Five`, i.e. -1 mod 6).
- **A single move has order 12.** 12 consecutive same-face turns return to solved
  (verified); `R` and `R'` are true inverses of each other, not self-inverse.
- **Opposite-face turns are equivalent up to reorientation.** `R L'` is a pure
  whole-ball rotation (every face shows one uniform color afterward, just relabeled) —
  not a scramble. This generalizes: any face's turn ≈ its opposite's turn, differing
  only by which way the whole ball ends up facing.
- **Corners can only ever be permuted, never twisted**, by any sequence of moves
  whatsoever. Proof sketch: every operation touching a corner is a 180° rotation about
  one of the 3 cube axes; these three rotations plus identity form a Klein four-group
  (abelian, every non-identity element an involution), and no non-identity element of
  that group fixes an all-±1 corner position. So a corner returning to its original
  slot must have accumulated the identity transform — no twist is possible. This means
  a "corners-first" method, if viable, reduces to a pure 8-element permutation problem
  with zero orientation bookkeeping.
- **Edges are heavily paired/constrained**, much more than a standard 3x3 (this
  puzzle's 180-only move set is structurally similar to the "double-move-only" 3x3
  variant). Empirically confirmed (zero violations across tens of thousands of
  reachable states): two edges sharing one color, whose other two colors are opposite
  each other (e.g. Red-Yellow and Purple-Yellow, since Red/Purple are opposite), are
  always either cube-opposite (antipodal slots) or face-opposite with the shared
  color's center adjacent to *both* or *neither* — never a mixed case. Their
  hex-orientations always match exactly. This is why large chunks of the ball "fall
  into place" once a few pieces are positioned correctly during manual solving.

## Source layout

- **`src/Lib.hs`** — the static representation and the turn engine.
- **`src/Search.hs`** — the constrained-search toolkit built on top of `Lib`.
- **`app/Main.hs`** — minimal demo (not load-bearing).
- **`test/Spec.hs`** — plain-assertion test suite (no framework — see Build notes).

## Static representation (`Lib.hs`)

Straightforward ADTs: `Color` (6 values), `HexOrientation` (`Zero`..`Five`, a plain
mod-6 accumulator of gear turns — *not* redefined relative to which color is listed
first), `HexEdge Color Color HexOrientation`, `Corner Color Color Color`, `Center
Color`, `Face FaceLocation Corner×4 HexEdge×4 Center`, `GearBall Face×6`. Convention
throughout: the first-listed color is whichever one faces the addressing face; for
corners the second color is the next one clockwise.

`FaceLocation` is `Up | Down | Left | Right | Front | Back` — note `Left`/`Right`
collide with `Prelude`'s `Either` constructors once you actually use them, so every
module that touches them starts with `import Prelude hiding (Left, Right)`.

`solvedGearBall` uses the fixed assignment Up=Yellow, Down=Orange, Left=Purple,
Right=Red, Front=Blue, Back=Green.

## Geometry engine (internals of `Lib.hs`)

Pieces are addressed by position in a fixed ±1 coordinate frame (`type Vec3 =
(Int,Int,Int)`, Right=+x/Up=+y/Front=+z and opposites). A corner sits at an all-±1
point, an edge at a point with exactly one 0, a center at a unit vector. This lets
every one of the 6 possible face turns share the same rotation math (`faceSpin180`
for the 180° own-layer part, `slice90CW` for the 90° slice-drag part) instead of
hand-transcribing 6 separate permutations.

`BallState` is the internal position-indexed representation (`Map Vec3 (Map Vec3
Color)` for corners, similar-with-orientation for edges, `Map Vec3 Color` for
centers) — used for all move computation. `toBallState`/`fromBallState` convert
to/from the public `GearBall`. `applyBaseTurn :: FaceLocation -> Bool -> BallState ->
BallState` is the one real primitive; everything else composes it.

`reorientBall`/`reorientBallState` pick the whole ball up and re-hold it 90° from a
given face — *not* a puzzle move (no gears mesh), so it applies uniformly to every
corner/edge/center and never touches a `HexOrientation`. Useful for constructing test
scenarios with a piece on a specific axis.

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
`maxLen`) for the *shortest* sequence, drawn only from the given move alphabet,
reaching a state satisfying an arbitrary predicate. Prunes only the immediate-inverse
case (never appends a move that's the direct inverse of the previous one — legitimate
here since moves have order 12, not order 2/4 like a 3x3, so don't reuse 3x3 pruning
intuitions). No commutator support yet — this is intentionally the "naive" version.

The predicate is where all the design care lives. Two genuinely different kinds of
question keep coming up, and they need different tools:

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

## Verified algorithms found so far

- **Yellow-ring 2-edge-swap fix (color-anchored):** given a "ring" of 4 edges around
  one center where 2 are correctly home and the other 2 are swapped with each other,
  `L U L' B'` fixes it — verified reusable across 8+ independently-generated
  scrambles regardless of which absolute face the ring's center ended up on. Built
  with `edgeHome`/`preserves`(all 6 centers) + the 2 known-good edges.
- **Genuine positional FR/FL swap:** protecting FU, FD, and all 6 centers exactly,
  swapping FR↔FL as an explicit `Relabeling` (F stays F, R↔L), the shortest fix is
  `U2 F U2 F` (6 moves) — confirmed longer than naive attempts, consistent with this
  being a real parity-type fix.

These two are examples of the same underlying situation described two different ways
(color-relative vs. positional) — worth keeping both framings in mind when searching:
color-relative goals tend to find shorter, more "opportunistic" fixes that exploit
whatever else happens to already be in place; positional goals find the fix for the
literal request, which is usually longer but means what it says.

## Solving-method exploration status (empirical, not yet systematized)

- The "build a layer/cross freely, ignore the rest" 3x3 intuition partially applies
  but is complicated by every move disrupting roughly 2/3 of the ball (vs. 1/3 on a
  3x3) — a "building phase" needs the constrained search above to find safe sequences,
  it's not generally achievable by casual/intuitive manipulation the way early 3x3
  layers are.
- Placing one ring edge correctly (position, ignoring hex-orientation) forces the
  entire ring correctly into place, given correct centers — a large amount of
  structure "falls out for free," per the edge-pairing facts above.
- A real parity-like obstruction exists: sometimes 2 of a ring's 4 edges go in easily
  and the other 2 are stuck swapped with each other, un-fixable by naive 2-edge
  manipulation alone (see the verified 6-move fix above).
- No general "phase 1 / phase 2 / phase 3" method has been nailed down yet. Corners
  are known to need only permutation-fixing (never orientation, see the proof above),
  which should make a corners-first or corners-last phase simpler than the equivalent
  on a 3x3.

## Build notes

- `stack build` / `stack test`. `stack.yaml` uses `resolver: ghc-9.6.7` with no
  curated snapshot — only GHC boot packages (`base`, `containers`) are reliably
  available without hitting Hackage, which is why the test suite is plain
  `IO`-and-assertions rather than hspec.
- `package.yaml` is the source of truth; stack auto-regenerates `gear-ball.cabal` from
  it on build (has its own embedded hpack even though no standalone `hpack` binary is
  installed) — don't hand-edit the `.cabal` file, and don't bother listing modules
  explicitly in `package.yaml`, they're auto-discovered from `src/`.

## Natural next steps

- Extend `search` with commutator support — naive IDDFS is fine through ~depth 6-7 but
  gets slow beyond that, and "touches almost nothing else" algorithms (the eventual
  goal for a late "algorithm phase") likely need more moves here than the 3x3
  equivalent, given how much a single move disturbs.
- Systematize the corners-first idea now that orientation is provably a non-issue for
  corners.
- Build out more `Relabeling`-based searches for other candidate building-phase moves,
  now that it's established as the correct tool for "move piece X to Y" requests.
