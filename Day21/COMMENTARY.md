# Day 21: Monkey Match

[Puzzle text](https://adventofcode.com/2022/day/21)

[My code](https://github.com/DERAlfons/aoc2022/blob/master/Day21/Main.hs)

On the twentyfirst day, the task was basically to solve an equation with a large number of terms.

It simplified to a linear equation with one variable though, so it was actually very easy to solve.

I had some concerns about potential rounding issues, because strangely enough the puzzle text did
not specify how division would be handled. But that ended up not mattering at all, probably because
all divisions were between integers where the dividend is evenly divisible by the divisor. I did
not check if that is actually the case, but i highly suspect it.