ICFP 2018, team last replicator ration.

The idea I had early on was that routing paths through a cube this large wasn't going to be easy, so I decided very early to start considering the board in smaller
cubes.  I thought a lot the first day about how disconnected parts in each of the cubes could interact and realized that there was no 100% certain way to sequence
building the cubes whole, or even strings of shapes woven through them.  It hit me that since I had already been thinking about shapes that I could do local reasoning
about them and route a connection from the ground to every shape in the system, one cube at a time, with the goal of being able to turn off the heater before doing
the bulk of the work.  This part worked well.  I didn't fully build the code needed to explore how to populate every cube once we've started annealing new material
onto the grounding routes that are prepared early on so it missees cubes from time to time.

I didn't have time to do anything with parallelism, but since the board is divided into discrete units, it should be relatively simple as long as the plies are
synchronized and the individual printers aren't in adjacent cubes.

I used almost all the time, and feel like I made effective use of most of it.  I had very little code that wasn't being used for something in the end, unlike other
ICFP problems I've taken on.

Thanks for organizing, this was a really great problem.

