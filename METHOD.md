# Solving method

The solution consists of the following phases:
1. Slice 1 permutation and orientation
2. Slice 2 permutation and orientation
3. Hex-oreientation for slices 1, 2, and 3
4. Edge permutation algorithms on last slice 
5. Corner permutations

# step 1  
First, simply choose a center to attempt to solve a "cross" at. For example, choose the yellow center. Find the red-yellow edge. Slot it in between the red and yellow centers so the wings of the edge are lined up (hex-orientation does not matter here). This is basically as easy as doing it on a 3x3, but you may have slight difficulty tracking the edges and centers together in some circumstances. Still, this should take a few moves.

After doing this, you will notice the opposite edge is also in position between the purple and yellow centers, also in the correct orientation (and also with the same hex-orientation as the red-yellow edge). Indeed, every edge along this entire slice (orange-purple and orange-red) will be in the correct position and orientation, with identical hex-spin.

At this point, you have essentially solved slice 1, except for hex-spin, which is easy to do later.

# step 2
Repeat for another slice, say the yellow-blue slice. Find the yellow-blue edge, attempt to slot it into position without messing up the red-yellow and purple-yellow edges. This is slightly trickier, but again it is no harder than attempting to slot the last edge into a cross in a normal 3x3. You may need to rotate the yellow face to "make room" for the blue-yellow edge before rotating back.

Again, once this is accomplished, the entire slice of edges (orange-blue and orange-green) will be in position and correct orientation, with identical hex-spin.

# step 3 - spin
Spin can be fixed for an entire slice pretty independently of everything else on the gear-ball, so it can be done later, but now is a good time to do it. Pick a slice, say the red-yellow slice. Orient it at M. Literally just spin R until the middle slice hex-spin is solved. Turning R4 returns everything on the ball to its original location except it spins the 4 gears of the M slice 2 notches CCW. L4 (or R8) sends them 2 notches CW. As a useful mnemonic, view the gear and consider which way it needs to spin to become correct more quickly (i.e. it will need to rotate 2 notches CW or 4 CCW, so 2 CW would be "faster"). Then start turning either L or R, which ever starts spinning the gear in that "faster" rotational direction. That will be the correct turn.

This can be repeated for the other two slices quite easily. As stated before, this step can also be done later, but it makes recognition slightly easier.

# step 4 - last slice edges
The last slice will have edges permuated in one of three ways
 - correctly, nothing to be done
 - 2 pairs of face-opposite edges swapped (such as FU sent to BU, FD sent to BD)
 - 2 pairs of ball-opposite edges swapped (such as FU sent to BD, BU sent to FD)

The algorithm for the second case can be applied twice to solve the third case too, or you can learn both algorithms.
LUF'L'D'F

ULFU'R'xU'

# step 4 - corners
The corners will be in one of 4 states (or two, depending on how you look at it). Either solved, or flipped about one of the 3 axes. This step is simple.

Point the "solved" face at you, at F. Then do
R'URx'URx'U'R'xU'

# other ideas
Given the simplicity of the last two phases, it is probably easy to do them together at once. Recognition might be a little difficult, but there can't be too many cases.

I also think that hex-spinning is so simple it could easily be manipulated during other phases too. Perhaps hex-spinning and last-slice permutation could be one step, for example. Though the fact that a hex-spin requires 4 consecutive turns of one face makes me think perhaps not. I do think hex-spinning can be influenced in the 2nd slice pretty easily as well. There are likely situations where you can insert the last edge of the face with one turn, say R, but you instead decide to do R'3 to properly orient the middle slice. More complicated ideas along these lines are certainly possible. Maybe it would be possible and easy to just focus on fixing last slice permutation while inserting the second slice, this would be more bang for your buck.

# ergonomics
R and L turns are the easiest. U is probably next, FB and D are harder and awkward and as such I avoid them in algorithm searching. To prevent extra bad turns, you can realize an R' F as R' x U. This is advantagenous because R' x is actually done in one motion, in fact it is closer to doing a half R' turn and a half L' turn at the same time.  This is how you should do any such RL moves followed by an x rotation, the following x rotation will always counteract the R or L move (i.e. always R' x never R' x') to keep the F face center unchanged. This is done purposefully.
s