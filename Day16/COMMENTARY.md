# Day 16: Proboscidea Volcanium

[Puzzle text](https://adventofcode.com/2022/day/16)

[My code](https://github.com/DERAlfons/aoc2022/blob/master/Day16/Main.hs)

The sixteenth day's task was to find an optimal order in which to open some valves to release a
maximal amount of pressure.

This task was much harder than the previous ones. A first thing to note is, that most valves have a
flow rate of zero, so they only need to be considered as pathways, not for opening. Now, for
finding the optimal solution, the intuitive approach would be, to just check all the possible
orders of valves and choose the one, which releases the most pressure. That is not possible though,
because there are too many combinations. So, in my initial solution, i considered partial paths
between the valves, and calculated two values for each of them: Firstly, the minimal pressure
release, which assumes that no further valves are opened along this path. Secondly, the theoretical
maximal pressure release, obtained by assuming that all the remaining valves form a straight line
in descending order of value with a single step between them. Partial paths, where the theoretial
maximum is lower than the minimum of some other path, can be discarded early.

When i reworked the code, i realized, that i had (once again) written something that is more or
less the dijkstra algorithm. That was not obvious to me at first, because we are looking for a
maximum and the dijkstra algorithm finds a minimum. But the problem can be reformulated, so that we
are looking for a minimum in the difference between the theoretical maximal pressure release and
the actual pressure release. I rewrote my code to make use of some prepackaged dijkstra algorithm.

In the second part of the task, where there are two guys going around and opening valves (me and
the elephant), there arises an interesting problem, which is that for each state, there is also
some kind of a flipped state, where everything is the same except me and the elephant are swapped.
Those two states are not essentially different, so when looking for an optimal solution only one of
them needs to be considered. Currently, my code considers both of these states, which makes it
inefficient by a factor of two. In a more complex scenario with even more guys going around, that
factor would increase significantly. With three guys, it would already be six, as it is the
factorial of the number of guys. Working around that by considering only one of the symmetrical
states would be a bit tricky, so i did not do it.

My initial solution for the second part was much faster than the reworked one, but it assumed that
me and the elephant open the same number of valves in the optimal solution. That is a reasonable
assumption and it works out in this case, but in general, the optimal solution does not necessarily
have this property.