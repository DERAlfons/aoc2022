# Day 25: Full of Hot Air

[Puzzle text](https://adventofcode.com/2022/day/25)

[My code](https://github.com/DERAlfons/aoc2022/blob/master/Day25/Main.hs)

On the twentyfifth day, the task was to add some numbers given in an unusual representation.

The represantation was a base 5 system, but with digits ranging in value from -2 to 2 instead of 0
to 4. Their addition works very similar to addition in regular base 5: In regular base 5, for each
place, you calculate the sum of the two digits corresponding to that place. If that sum is 5 or
more, you carry a 1 over to the next place. In the number system of this task, you also calculate
the sum of the two digits for each place. But then, you carry over a 1 if the sum is 3 or more, and
you also carry over a -1 if the sum is -3 or less.