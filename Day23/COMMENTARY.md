# Day 23: Unstable Diffusion

[Puzzle text](https://adventofcode.com/2022/day/23)

[My code](https://github.com/DERAlfons/aoc2022/blob/master/Day23/Main.hs)

On the twentythird day, the task was to simulate the movement of entities on a two-dimensional
grid.

A basic idea would be to use a two-dimensional array as a model for the grid. It would be a bit
tricky though to determine the necessary size of the array, as the elves expand the area which they
occupy. So instead, i stored the positions of the elves as a `Set` of points.