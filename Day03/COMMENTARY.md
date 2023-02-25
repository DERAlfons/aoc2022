# Day 3: Rucksack Reorganization

[Puzzle text](https://adventofcode.com/2022/day/3)

[My code](https://github.com/DERAlfons/aoc2022/blob/master/Day3/Main.hs)

The third day's task was essentially to find the elements, which all lists in a given
set of lists have in common. My solution to that was pretty much straight forward.

The most interesting thing to note is that i added the function `groupsOf`, which splits
a list into multiple sublists of a given length, to my utility library.