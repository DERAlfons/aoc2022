# Day 8: Treetop Tree House

[Puzzle text](https://adventofcode.com/2022/day/8)

[My code](https://github.com/DERAlfons/aoc2022/blob/master/Day08/Main.hs)

The task on the eighth day day was to do some calculations regarding the visibility of trees on a
given height map of trees.

The obvious approach to this problem would be to model the tree height map as a two dimensional
array and to do some iterative calculations regarding on the rows and columns of that array. That
is not a very natural thing to do in Haskell, so at first i wrote a solution in C. To be more
precise, i wrote some C functions and called them from Haskell code, which worked surprisingly
well.

But i did not really like that solution, because the C code was somewhat long and parts of it
seemed kind of redundant. So i decided to let efficiency be an afterthought, and to write a
solution in Haskell using `List`s. At some point, i went a bit overboard with the idea of not using
indexing on the lists, and used a wild combination of `zip`s and `unzip`s to create essentially a
two line solution, which was all but totally unreadable. Finally, i settled with a solution which
had a few more lines, but which i found to be very concise and readable.

[Here](https://github.com/DERAlfons/aoc2022/blob/master/Day08v0) is my initial Haskell + C code.