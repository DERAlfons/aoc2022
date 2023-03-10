# Day 17: Pyroclastic Flow

[Puzzle text](https://adventofcode.com/2022/day/17)

[My code](https://github.com/DERAlfons/aoc2022/blob/master/Day17/Main.hs)

On the seventeenth day, the task was to simulate some falling blocks, similar to the Tetris video
games, but without rotating and without disappearing lines.

In the second part of the task, the number of falling blocks to simulate is very high (`10 ^ 12`).
It would take a long time, to simulate each falling block individually. Instead, one can take a
look at the long term behaviour of the falling rocks and the wind. Since both repeat periodically,
one can expect that after some time they synchronize and create a repeating pattern in the tower of
rocks. In this case, the repeating pattern is already established during the first round of wind.
These repeating patterns always contain the same amount of rocks and add the same amount of height
to the tower, so they can be used to quickly determine the height of a tower with a large number of
rocks. Of course, there are some rocks before the start of the repeating pattern, and also some
remaining rocks after the last instance of the repeating pattern. Those still have to be simulated
individually. To do that, one can simulate a single tower consisting of the rocks at the start and
the remaining rocks at the end, with all the repeating pattern in between them removed.