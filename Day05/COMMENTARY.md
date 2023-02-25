# Day 5: Supply Stacks

[Puzzle text](https://adventofcode.com/2022/day/5)

[My code](https://github.com/DERAlfons/aoc2022/blob/master/Day05/Main.hs)

The task of the fifth day was in essence to move elements between lists.

This was the first task, which was a bit harder to do in Haskell than it would have been
in some imperative language. In such a language, one could have stored the lists in an array
and manipulated the fields of the array according to the given instructions. In principle,
the same can be done in Haskell by using a mutable array (`MArray`). But in my solution,
i instead used a `Map`. The main drawback of using a `Map` instead of an array is, that the
complexity of accessing elements is O(log n) instead of O(1). In other scenarios, a possible
benefit of `Map` can be that it uses less space when dealing with sparcely populated arrays.
But the actual reason i used `Map` here is, that i am more familiar with it than i am with
mutable arrays.

P. S.: I ended up also implementing a solution using a mutable array, which can be found
[here](https://github.com/DERAlfons/aoc2022/blob/master/Day05v2/Main.hs)