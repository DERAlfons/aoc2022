# Day 6: Tuning Trouble

[Puzzle text](https://adventofcode.com/2022/day/6)

[My code](https://github.com/DERAlfons/aoc2022/blob/master/Day06/Main.hs)

The sixth day's task was to find substrings with distinct characters in a string.

This one was kind of easy. Nothing much to say about it.

When i reworked the code, i added some exception handling for the case when
no valid marker substring is found. To help with that, i added the function
`maybeToIO` to my utility library, which raises an `IOException` when called
with `Nothing`, and otherwise returns the contained value. I might modify
this function in the future, so that it can raise other types of exceptions as well.