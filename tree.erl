-module(tree).
-export([sumTree/1, numElements/1, average/1, lessThan/2, grtEqThan/2, qs/1, elements/1, sortTree/1, median/1, makeTree/3, subTree/2]).

makeTree(empty, root, nil) -> nil;
makeTree(_, root, InsertedNumber) -> {InsertedNumber, nil, nil};
makeTree( {CurrentNumber, Left, Right}, Where, InsertedNumber) ->
  case Where of
    [r]   -> {CurrentNumber, Left, {InsertedNumber, nil, nil}};
    [l] -> {CurrentNumber, {InsertedNumber, nil, nil}, Right};
    [r | Tail] -> {CurrentNumber, Left, makeTree(Right, Tail, InsertedNumber)};
    [l | Tail] -> {CurrentNumber, makeTree(Left, Tail, InsertedNumber), Right}
end.

subTree(Tree, []) -> Tree;
subTree({_, Left, Right}, [Head | Tail]) ->
  case Head of
    r -> subTree(Right, Tail);
    l -> subTree(Left, Tail)
  end.


sumTree(nil) -> 0;
sumTree({Number, Left, Right}) -> Number + sumTree(Left) + sumTree(Right).

numElements(nil) -> 0;
numElements({_, Left, Right}) -> 1 + numElements(Left) + numElements(Right).

average({Number, nil, nil}) -> Number;
average(Tree) -> sumTree(Tree) / numElements(Tree).

elements(nil) -> [];
elements({Number, Left, Right}) -> elements(Left) ++ [Number]  ++ elements(Right).

lessThan(List, Arg) -> [ X || X<-List, X<Arg].
grtEqThan(List, Arg) -> [ X || X<-List, X>=Arg].
qs([]) -> [];
qs([Pivot|Tail]) -> qs(lessThan(Tail,Pivot) ) ++ [Pivot] ++ qs( grtEqThan(Tail,Pivot)).

sortTree(Tree) ->
  List = elements(Tree),
  qs(List).

median({Number, nil, nil}) -> [Number];
median(Tree) ->
  List = sortTree(Tree),
  NumberOfElements = numElements(Tree),
  Half = NumberOfElements div 2,
  case (NumberOfElements rem 2) of
    0 -> (lists:nth(Half+1, List) + lists:nth(Half, List))/2;
    1 -> lists:nth(Half+1, List)
end.