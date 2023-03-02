# Day 15: Beacon Exclusion Zone

[Puzzle text](https://adventofcode.com/2022/day/15)

[My code](https://github.com/DERAlfons/aoc2022/blob/master/Day15/Main.hs)

On the fifteenth day, the task was to find a point which is not contained in any of some
diamond-shaped areas.

A simple approach to this would be, to go through all possible points and check whether they are in
any of the areas. There are around `1.6e13` possible points though, so that would take a long time.
What i did in my solution was to go through the possible points line by line and calculate the
intersections of those lines with the diamond-shaped areas, and check if there are any points on
those lines not contained in any of the intersections.

A logical next step and an even faster solution would be, to find the point by calculating the
intersections between rectangles. What makes this a bit tricky is, that the areas are tilted
relative to the coordinate system. But i still think, that it could be done.