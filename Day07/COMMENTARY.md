# Day 7: No Space Left On Device

[Puzzle text](https://adventofcode.com/2022/day/7)

[My code](https://github.com/DERAlfons/aoc2022/blob/master/Day07/Main.hs)

On the seventh day, the task was essentially to recreate a file system tree from a commmand line
session log.

This was the first task with kind of medium difficulty. In my initial solution, i first parsed
each line of the log into a custom datatype, and then traversed the parsed log and used pattern
matching to recursively assemble the file system tree.

To help with the parsing of the lines, i created my own parser library. The first version of that
library only had one function, which took a list of regular expressions and functions to act on
the match sublists, and parsed a string according to the first matching regular rexpression of
that list. During the following days, i would extend and rework this library.

When i reworked the code for this day, my parser library was powerful enough to combine the two
steps of parsing the lines of the log and assembling the file system tree into one single step,
which i find to be very elegant.

Improvements could be made regarding the robustness of the code. At the moment, it can only handle
`cd` commands, where the argument is either a directory in the current directory or `..`. This
could be extended to also handle commands like `$ cd ../..`. Also, the code assumes that each
directory is traversed only once. Another improvement would be, to check if the filesystem is
traversed completely during the command line session, by comparing the traversed directories with
the directories in the output of `$ ls`, which are currently just dropped.