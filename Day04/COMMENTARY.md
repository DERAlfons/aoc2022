# Day 4: Camp Cleanup

[Puzzle text](https://adventofcode.com/2022/day/4)

[My code](https://github.com/DERAlfons/aoc2022/blob/master/Day04/Main.hs)

On the fourth day, the task was to find intersections between pairs of integer ranges.

The condition i used to check wether one of the ranges `r1` and `r2` fully contains
the other was the rather unwieldly `(start r1 <= start r2 && end r2 <= end r1) || (start r2 <= start r1 && end r1 <= end r2)`,
which can be understood as "`r1` contains `r2` or `r2` contains `r1`". I felt like
this could be simplified to something like `(start r1 <= start r2) == (end r2 <= end r1)`, but
this does not quite work because of the edge cases where both ranges start or end at the same value.
For the same reason, a function with an argument flip also does not make the check simpler.

The condition to check wether the two ranges `r1` and `r2` have any overlap can be expressed
a bit simpler in `start r1 <= end r2 && start r2 <= end r1`. To see how this checks for overlap,
it is easier to look at its negation, which can be transformed to `end r1 < start r2 || end r2 < start r1`,
and to see that that is true exactly if `r1` and `r2` do not overlap.

In my initial solution, i directly used some regex matching for parsing the input.
I later changed that to using my parser library, which i started writing on day 7 of the
Advent of Code challenges.

Furthermore, i added a `count` function to my utility library, which counts the elements
of a list which satisfy a given predicate. Originally, i wanted the function to just count
occurences of a given element in a list. But that version would not be useful in the most
common case, which is to count `True`s in a list of boolean values, which itself was generated
by mapping a predicate on some list. In that case, `count True $ map predicate list` would
be longer than `length $ filter predicate list`. With the version of `count`, which i ended
up implementing, this is shortened to `count predicate list`.