# Day 22: Monkey Map

[Puzzle text](https://adventofcode.com/2022/day/22)

[My code](https://github.com/DERAlfons/aoc2022/blob/master/Day22/Main.hs)

The task on the twentysecond day was to follow a path on a map with obstacles, where the edges of
the map are connected in some way.

The first part, where opposite edges are connected, was pretty simple. The second part, where the
edges are connected like on the surface of a cube, was a bit harder. In my solution, i manually
coded all the edge transitions. Luckily, i did not make any mistakes there, or rather, not any
mistakes which would affect the solution.

I thought about some other ways to model the map for the second part, for example a model where the
position always stays on top of the cube and on edge transitions the cube rotates, rather than the
position. But all the ideas i had were a bit tricky to implement.