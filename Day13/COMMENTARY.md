# Day 13: Distress Signal

[Puzzle text](https://adventofcode.com/2022/day/13)

[My code](https://github.com/DERAlfons/aoc2022/blob/master/Day13/Main.hs)

The thirteenth day's task was to compare nested lists of integers, where the lists can have mixed
levels of nesting.

This was my favourite task of this year's Advent of Code. Reading the nested lists of mixed depth
required some kind of recursive parsing. When i worked on such kinds of parsing in the tasks of
previous Advents of Code, i always felt like my code was somewhat complicated and had redundant
parts with multiple functions calling each other. This time though, i wanted to simplify the code
as much as possible. To do this, i extended the parser library which i started on day 7. I ended up
with a `Parser` type, which i made an instance of `Functor`. I did not make it an instance of
`Applicative` initially, but i felt like it should be. Later, i went back to work that out, and
that was when i really understood what applicative functors are and how their operators work.

For comparing the nested lists, i created a datatype and made it an instance of `Ord` according to
some very simple rules, which was satisfying as well.