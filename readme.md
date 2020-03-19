# OpenX Intership Task - Variant 1

## The task was to create:
 - objects capable of mapping the binary tree (node values are integers),
 - functions (sum, average, median) operating on any subtree,
 - tests showing the correctness of implementation.

## Files:
 - *tree.erl* - implementation of main functions (sumTree, average, median) and auxiliary functions (makeTree, subTree, numElements, elements, lessThan grtEqThan, qs, sortTree),
 - *tree_tests.erl* - tests of every function implemented in tree.erl (to run all tests use: *tree_test:test().*).

## Example:
![](./OurTree.PNG)

```erlang
> OurTree = {5,{3,{2,nil,nil},{5,nil,nil}},{7,{1,nil,nil},{0,{2,nil,nil},{8,nil,{5,nil,nil}}}}}.

> tree:sumTree(OurTree).
38

> tree:average(OurTree).
3.8

> tree:median(OurTree).
4.0

% Operating on a subtree.

> Subtree = tree:subTree(OurTree, [r,r]).
{0,{2,nil,nil},{8,nil,{5,nil,nil}}}

> tree:sumTree(Subtree).
15

> tree:average(Subtree).
3.75

> tree:median(Subtree).
3.5
```
