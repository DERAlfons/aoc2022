# Day 1: Calorie Counting

On the first day, the task was to find maximal values in a list of numbers.
Although a fairly simple task, there are already a few interesting things to consider.

* At some point during the last year i learned to use the `<$>` operator
  to read content from a file and transform it in a single line of code. Before then,
  i would have read it and transformed it in multiple lines, making it necessary
  to name all the intermediate results (naming things is hard!)
* I added the `explode` function to my utility library, which splits a list into
  multiple parts along a given seperator symbol. The obvious use case for such
  a function is to split a string along a character, which is typically `','`.
  But it also works really well for grouping lines in a textfile which are seperated
  by a blank line
* To find the 3 biggest elements of the calories list, my idea was to first sort
  the list. The problem here is, that `sort` sorts the list in ascending order,
  but to easily access the biggest elements, i need the list to be sorted in descending order.
  I considered a few options on how to deal with this:
  - `sort` then `reverse`: An ok solution, but a little bit wasteful, because
    the `reverse` computation would not be necessary, if the list was just directly
    sorted in descending order
  - `sortOn negate`: Would sort the list in descending order, but i do not like
    it at all. It can only work on numerical types, and even then, it does not work
    on unsigned integers for example (because `negate` maps unsigned integers to their
    complement, so it would work for positive unsigned integers, but `negate 0` is `0`,
    so `0` would be considered the biggest element)
  - `sortBy (flip compare)`: This is what i ended up using. But i did not like the
    look of it. To me, it is not obvious, that this sorts a list in descending order.
    So i added it to my utility library and called it `sortDesc`. I also added `sortAsc`
    as an alias for `sort`, for good measure.
  Finally, traversing the list once and keeping track of the 3 biggest elements would
  be faster than sorting it. But that is an optimiziation, which can be done when necessary.