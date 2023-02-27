# Day 11: Monkey in the Middle

[Puzzle text](https://adventofcode.com/2022/day/11)

[My code](https://github.com/DERAlfons/aoc2022/blob/master/Day11/Main.hs)

The task on the eleventh day was to move items between queues according so some divisibility rules.

This task was the first one where a bit of mathematics was needed to solve it. In the second part
of the task, the worry levels take large values. One would need something on the scale of
`2 ^ 10000` bytes to store these values, which is of course much more than any computer could
handle. But fortunately, instead of calculating the actual values, it is enough to keep track of
their divisibilities in regards to a few numbers. To do that, one can calculate the modulus with
regard to the least common multiple of those numbers. In this case, the numbers are all distinct
primes, so their least common multiple is just their product.

Another possible approach would be, to keep track of the modulus with regard to each of the numbers
seperately. It could be interesting to check if there are cases, where this approach is more
efficient. Intuitively, i would assume there are none.