#  GearBall algorithms

Every algorithm here was found by `Search.search` (iterative deepening over the
12 primitive moves) or by meet-in-the-middle (more efficient when the end state is fully known).

Notation is the project's usual `RLFBUD` with `'` for counter-clockwise — recall
that every letter is already a 180° turn, so `R` here is what would feel like an `R2` on a classic 3x3.

Algorithms are designed to manipulate very specific pieces in certain ways and leave everything else on the ball untouched.

## Edges

### Two-pair edge swap — 6 moves

```
L U F' L' D' F
```

Swaps `FU` ↔ `BU` and `FD` ↔ `BD`. In each case the F-facing color ends up
facing B (and vice versa) while the U/D-facing color stays on U/D.
As a "bonus", the hex-edges retain their original hex-orientation, meaning this can be done after fixing hex-orientation across the entire ball.

This does permute corners (two 4-cycles), so this is only useful before solving corners.

6 moves is optimal. Iterative deepening exhausted lengths 0–5 first, this is pretty easy to prove.
There are obviously many similar algorithms, most with the same general "almost" commutator shape.

I choose to display this particular algorithm for ergonomics. Identifying edge permutations is easiest on the M-slice.
R/L and U moves are smoother than D and much smoother than F/B. Yet even with two F moves, this is about as smooth as this alg can get.

### Diagonal edge swap — 6 moves

```
U L F U' R' F'
```

This is the "other" edge permutation algorithm. The "straight" alg could be done once,
followed by a rotation of the whole cube, and then done again to perform a "diagonal" edge swap.
But this is a simpler algorithm.
This algorithm swaps `FU` ↔ `BD` and `FD` ↔ `BU`, diagonally across the ball. Again hex-edge orientation doesn't change anywhere. Again, corners are permuted. And again, 6 moves is optimal.

Ergonomically, this is a little better than the above alg because it avoids a D move. But it's not much more elegant.
Both algs are suitable simply because of their shortness more than anything else.

## Corners

### Corner cycling

After all edges are solved, corners could either be solved or every corner could be rotated 180°
about one cube axis while the rest of the ball stays put. Thus, there is really only one case (or solved).

```
R' U R B R D' R' B'     (LDB RUB)(LDF RUF)(LUB RDB)(LUF RDF)    half-turn about F–B
```

This is basically the best one to learn ergonomically. Hold the ball so the axis the corners are flipped about points **at you**, that is, the face you are looking at is solved.
Then perform this as `R' U R x' U R x' U' R' x U'`, which touches nothing but R and U.
The ergonomic trick is that `R x'` is easy to do. Remember that `R` is really a double rotation, more like an `R2`.
So doing `R x'` in *one* motion means you don't have to rotate your right wrist all the way around.
The extra benefit is that this puts the ball in a different orientation so that those subsequent moves become `U`.
Everything cancels out quite nicely. This is a really smooth operation.

8 moves is minimal. This can be proved by meet-in-the-middle search, which is more efficient.
You search 4 moves forward from the "corners permuted" state, and 4 moves backwards from the "ball solved" state, and if any positions overlap you have an algorithm.

### Consequences for solving methods

These algorithms make the idea that solving corners last seem logical. Solving edges is
such a strong constraint that the corners are only left in 4 possible states (3 different axis rotations, or solved).
And solving edges is easy, and the edge algorithms are pretty clean as well.

Since there are literally only three algorithms to remember here, a serious attempt to
speed-solve this would probably involve reducing these two steps into one: solve the last-slice
edges and all corners at once.

This must be fewer than 16 different cases. There are 4 last-slice edge arrangements —
solved, two different straight swaps, and the diagonal swap. And there are 4 corner states
per edge state. So it would be completely feasible to memorize these states, I think only
recognition would be awkward.

Searching proves they collapse into 9 unique states. I will simply provide known algorithms for these.
These are minimal and also chosen to be somewhat ergonomic.

| # | last-slice edges | corners | algorithm | n | perform as |
|---|---|---|---|---|---|
| 1 | solved | solved | — | 0 | — |
| 2 | solved | half-turn (any axis) | `R' U R B R D' R' B'` | 8 | `R' U R x' U R x' U' R' x U'` |
| 3 | straight swap | (LDF RDB)(LUB RUF) | `L' U' L U L F R' F'` | 8 | `L' U' L U L x U R' U'` |
| 4 | straight swap | (LDB LUF)(LDF RUF)(LUB RDB)(RDF RUB) | `L' U' F L U B'` | 6 | `L' U' F L U B'` |
| 5 | straight swap | (LDB RDF)(LUF RUB) | `L U L U' L' B' R' B` | 8 | `L U L U' L' x' U' R' U` |
| 6 | straight swap | (LDB RUB)(LDF LUB)(LUF RDF)(RDB RUF) | `L U B' R' U' B` | 6 | `L U B' R' U' B` |
| 7 | diagonal swap | (LDF LUB)(RDB RUF) | `U L F L F' L' U' R'` | 8 | `U L x U L U' L' x' U' R'` |
| 8 | diagonal swap | (LDB RDF)(LDF RUF)(LUB RDB)(LUF RUB) | `U' L' B' U R B` | 6 | `U' L' B' U R x' U` |
| 9 | diagonal swap | (LDB RUB)(LDF RDB)(LUB RUF)(LUF RDF) | `U L F U' R' F'` | 6 | `U L F U' R' x U'` | 

The last slice is held as the L–R slice throughout, i.e. the four edges are `UF`,
`UB`, `DF`, `DB`.

So one-step last-slice + corners is very doable.s