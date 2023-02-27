# Day 12: Hill Climbing Algorithm

[Puzzle text](https://adventofcode.com/2022/day/12)

[My code](https://github.com/DERAlfons/aoc2022/blob/master/Day12/Main.hs)

On the twelfth day, the task was to find the shortest path up a mountain.

In the past, i would have written a path finding algorithm myself, which would have come more or
less close to being the Dijkstra algorithm. But this time, i just used a prepackaged Dijkstra
algorithm.

In the second part of the task, the goal is to find the shortest path between any of the lowest
points and the destination point. A simple approach would be, to find the shortest path for each of
the lowest points and then compare them. A more efficient way would be, to start the search at the
destination point and to stop as soon as it reaches any of the lowest points. Of course, when doing
this backwards search, the neighbourhood relationship between points has to be reversed.