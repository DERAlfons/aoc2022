# Day 14: Regolith Reservoir

[Puzzle text](https://adventofcode.com/2022/day/14)

[My code](https://github.com/DERAlfons/aoc2022/blob/master/Day14/Main.hs)

The task on the fourteenth day was to simulate sand falling through a two-dimensional cave system.

This task was kind of straight forward and there is nothing much to say about it, other than that i
used an `STArray` to keep track of the space filled with walls or sand. Using a `Map` or a `Set`
would have made the code a little more elegant, but also less efficient.