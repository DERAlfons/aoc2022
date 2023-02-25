# Day 9: Rope Bridge

[Puzzle text](https://adventofcode.com/2022/day/9)

[My code](https://github.com/DERAlfons/aoc2022/blob/master/Day9/Main.hs)

The ninth day's task was to in essence to simulate the movement of a rope, where the movement of
one end of the rope was given and the rest of the rope followed that end according to some rules.

The rules for how the rest of the rope followed the end were not trivial, but still kind of simple,
so this task was really easy.

In my solution, i used the `scanl'` function in two unrelated places (for updating each knot in the
rope, and for saving all intermediate rope states when going through the movement instructions).
I found that to be rather curious, because generally `scanl'` is a function that i use very rarely.