# Day 24: Blizzard Basin

[Puzzle text](https://adventofcode.com/2022/day/24)

[My code](https://github.com/DERAlfons/aoc2022/blob/master/Day24/Main.hs)

The twentyfourth days' task was to find the shortest path through a maze of moving obstacles.

At a first glance, this problem seems to be somewhat difficult, because of the moving obstacles.
But when you look at it in a way, where the time dimension becomes another spatial dimension,
and where each layer of that spacial dimension corresponds to a time step, it becomes just a
regular shortest path problem in three-dimensional space. You just have to consider, that you can
only move forward through the third dimension in this model. Also, the pattern of the obstacles
repeats periodically, so they can be stored in an array of finite size. To find the shortest path,
i used a pre-packaged dijkstra algorithm, as i did in some previous days' tasks.