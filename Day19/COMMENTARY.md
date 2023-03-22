# Day 19: Not Enough Minerals

[Puzzle text](https://adventofcode.com/2022/day/19)

[My code](https://github.com/DERAlfons/aoc2022/blob/master/Day18/Main.hs)

The nineteenth day's task was to find an optimal build order in which to build some
resource-gathering robots, to collect a maximal amount of resources.

To me, this was the hardest of the tasks by far, and while i did solve it, i am not satisfied with
my solution, because it is based on a heuristic, so it is not guaranteed to give the correct
result.

The basic idea would be to go through all the possible build orders and see, how many geodes they
produce. But the number of possible build orders increases exponentially with the time steps, so
there are too many possibilities to check. I tried to find a way to go through the possibilities in
a clever way to check as few of them as possible while eliminating the rest, similar to what i did
for the task on [Day 16](https://github.com/DERAlfons/aoc2022/blob/master/Day16/COMMENTARY.md), but
my efforts did not bear any fruit.

What i ended up doing was to reduce the number of possibilities every few (6) timesteps by picking
the 100 build orders which at that timestep have the most robots and the 100 build orders which at
that timestep have the most resources, and continuing only with those. By "most robots" and "most
resources" i mean most of the more advanced robots and resources, respectively. It seems plausible,
that the optimal build order would be in either of those categories, but of course there is no
guarantee, which is the reason why this solution sucks ass.