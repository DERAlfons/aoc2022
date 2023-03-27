# Day 20: Grove Positioning System

[Puzzle text](https://adventofcode.com/2022/day/20)

[My code](https://github.com/DERAlfons/aoc2022/blob/master/Day20/Main.hs)

The task on the twentieth day was to repeatedly shift the position of some Elements in a list.

The tricky part of this task was, that the order in which the elements are shifted is the order in
which they occur in the initial list, which can be very different from the order in which the occur
in the actual list after some shifting. I used a mixture of an array and a doubly linked list as a
data structure for that. The array part is used to retain the original order of the elements and
the doubly linked list part is used to shift elements in the list without having to relocate all
the elements in between its old and new positions (Which is what i did in my initial solution).

I also implemented the same thing in C++, using its `std::list` as a doubly linked list. The code
for that can be found [here](https://github.com/DERAlfons/aoc2022/tree/master/Day20v2).