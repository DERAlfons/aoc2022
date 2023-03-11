# Day 18: Boiling Boulders

[Puzzle text](https://adventofcode.com/2022/day/18)

[My code](https://github.com/DERAlfons/aoc2022/blob/master/Day18/Main.hs)

The task on the eighteenth day was to find the surface area of some three-dimensional shape.

In the second part of this task, a distinction had to be made between the surface border between
the shape and the exterior space, and the surface area between the shape and spaces fully
encompassed by the shape. For that, i created a new shape, which is a single body, completely
covers the original shape and completely fills some cuboid surrounding the original shape. The
exterior surface area of the original shape is the same as the interior surface area of the new
shape, which can be calculated as its total surface area minus the surface area of the surrounding
cuboid.