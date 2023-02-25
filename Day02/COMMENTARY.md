# Day 2: Rock Paper Scissors

[Puzzle text](https://adventofcode.com/2022/day/2)

[My code](https://github.com/DERAlfons/aoc2022/blob/master/Day02/Main.hs)

The task of the second day was to evaluate some Rock Paper Scissors matches.

In the first version of my solution, I essentially just hardcoded the outcomes
of all possible Rock Paper Scissors combinations that you could get in a match.
I did not like that approach very much, mainly because it would not scale well
to versions of the game with more hand signs (like [this one](https://www.umop.com/rps101.htm)).
So i made my data representation of Rock Paper Scissors hand signs an instance of `Enum`,
which allowed me to calculate the outcomes arithmetically. It also allowed me to do
some `Char` arithmetic, for parsing the encoded instructions. I am not really satisfied
with this solution either, because i am not a big fan of enums, but oh well.

Another thing to look at is the point system in this task, where different
hand signs are worth different amounts of points. The pure form of Rock Paper Scissors
has a trivial Nash equilibrium strategy, which is to randomly choose one of the hand signs
with equal probability. In the version from this task, it is not so clear what
the equilibrium strategy would be. Intuitively, one could assume that the distribution
shifts towards the higher valued hand signs. But that strategy could possibly
be countered by increasing the percentage of lower valued hand signs, which win
against the higher valued ones. It would be interesting, to actually calculate the Nash
equilibrium for this version of the game, and i might do that. It is not part of
the task though.