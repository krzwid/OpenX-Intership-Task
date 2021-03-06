-module(tree).
-export([makeTree/1, addLeaf/3, getSubtree/2,
  sumSubtree/2,  averageSubtree/2, medianSubtree/2,
  deleteBranch/2]).
-export([draw/1,get_verical_lines/2]).
-export([zip_new/0, get_verical_lines/1]).

makeTree(Root) -> {Root, nil, nil}.

addLeaf({Root, Left, nil}, [r], Number) ->
  {Root, Left, {Number, nil, nil}};
addLeaf({Root, nil, Right}, [l], Number) ->
  {Root, {Number, nil, nil}, Right};
addLeaf({Root, Left, Right}, [r | Tail], Number) when Right /= nil ->
  case addLeaf(Right, Tail, Number) of
    {error, Message} -> {error, Message};
    Subtree -> {Root, Left, Subtree}
  end;
addLeaf({Root, Left, Right}, [l | Tail], Number) when Left /= nil ->
  case addLeaf(Left, Tail, Number) of
    {error, Message} -> {error, Message};
    Subtree -> {Root, Subtree, Right}
  end;
addLeaf(_, _, _) ->
  {error, "Cannot insert root in this place"}.

getSubtree(Tree, []) -> Tree;
getSubtree({_, Left, Right}, [Head | Tail]) ->
  case Head of
    r ->
      case Right of
        nil -> {error,"Cannot get that subtree"};
        _ ->getSubtree(Right, Tail)
      end;
    l ->
      case Left of
        nil -> {error,"Cannot get that subtree"};
        _ -> getSubtree(Left, Tail)
      end
  end.

sumSubtree(Tree, []) -> sumTree(Tree);
sumSubtree(Tree, Direction) ->
  case getSubtree(Tree, Direction) of
    {error, Message} -> {error, Message};
    Subtree -> sumTree(Subtree)
  end.

%%private function summing whole tree
sumTree(nil) -> 0;
sumTree({Root, nil, nil}) -> Root;
sumTree({Root, Left, Right}) -> Root + sumTree(Left) + sumTree(Right).

averageSubtree(Tree, []) -> average(Tree);
averageSubtree(Tree, Direction) ->
  case getSubtree(Tree, Direction) of
    {error, Message} -> {error, Message};
    Subtree -> average(Subtree)
  end.

%%private function counting the average value of given tree
average({Root, nil, nil}) -> Root;
average(Tree) -> sumTree(Tree) / numerOfLeafs(Tree).

medianSubtree(Tree, []) -> median(Tree);
medianSubtree(Tree, Direction) ->
  case getSubtree(Tree, Direction) of
    {error, Message} -> {error, Message};
    Subtree ->
      case median(Subtree) of
        [Median] -> Median;
        Median -> Median
      end
  end.

%%private function counting median of given tree
median({Root, nil, nil}) -> [Root];
median(Tree) ->
  List = sortLeafs(Tree),
  NumberOfElements = numerOfLeafs(Tree),
  Half = NumberOfElements div 2,
  case (NumberOfElements rem 2) of
    0 -> (lists:nth(Half+1, List) + lists:nth(Half, List))/2;
    1 -> lists:nth(Half+1, List)
  end.

%%private function counting numbers of given tree
numerOfLeafs(nil) -> 0;
numerOfLeafs({_, Left, Right}) ->
  1 + numerOfLeafs(Left) + numerOfLeafs(Right).

%%private function returns sorted list of elements of the tree
sortLeafs(Tree) ->
  List = elements(Tree),
  lists:sort(List).

%%private function returns list of elements of the tree
elements(nil) -> [];
elements({Root, Left, Right}) ->
  elements(Left) ++ [Root]  ++ elements(Right).

deleteBranch({Root, Left, _}, [r]) ->
  {Root, Left, nil};
deleteBranch({Root, _, Right}, [l]) ->
  {Root, nil, Right};
deleteBranch({Root, Left, Right}, [r | Tail]) when Right /= nil ->
  case deleteBranch(Right, Tail) of
    {error, Message} -> {error, Message};
    Subtree -> {Root, Left, Subtree}
  end;
deleteBranch({Root, Left, Right}, [l | Tail]) when Left /= nil ->
  case deleteBranch(Left, Tail) of
    {error, Message} -> {error, Message};
    Subtree -> {Root, Subtree, Right}
  end;
deleteBranch(_, _) ->
  {error, "Cannot delete that branch"}.

%%showTree(nil) ->
%%  io:format("-- (nil)");
%%showTree({Root, Left, Right}) ->
%%  io:fwrite("~s~w~s~s~s~s",
%%    [
%%      "--",
%%      Root,
%%      lists:map(fun (X) -> "  |" ++ X end, showTree(Left)),
%%      "  `",
%%      showTree(Right),
%%      lists:map(fun (X) -> "  |" ++ X end, showTree(Right))
%%    ])
%%  .

get_verical_lines(Root) ->
  {L, R} = get_verical_lines(Root, zip_new()),
  tl(lists:reverse(L, R)). % concatenate zipper and cut off surplus empty list

get_verical_lines(nil, Zip) -> Zip;
get_verical_lines({V,L,R}, Zip) ->
  Zip1 = zip_right(get_verical_lines(L, zip_left(Zip))),
  Zip2 = zip_left(get_verical_lines(R, zip_right(Zip1))),
  zip_add(V, Zip2).

zip_new() -> {[], []}.

zip_add(V, {L, []   }) -> {L, [[V]    ]};
zip_add(V, {L, [H|R]}) -> {L, [[V|H]|R]}.

zip_left({[],    R}) -> {[], [[]|R]};
zip_left({[H|T], R}) -> {T,  [H |R]}.

zip_right({L, []   }) -> {[[]|L], []};
zip_right({L, [H|T]}) -> {[H |L], T }.

%%drawing tree
-record(box, {
  width = 0,
  draw = []
}).

draw(nil) -> [];
draw({V,L,R}) ->
  LB = draw_box(left, L, #box{}),
  {LPos, Label} = label_to_right(V, LB, []),
  RB = draw_box(right, R, boundbox(expand_box(LB), LPos+1)),
  [lists:reverse(X, [$\n]) || X <- [Label | RB#box.draw]].

draw_box(_, nil, Box) -> Box;
draw_box(Dir, {V,L,R}, Box) ->
  {LineT, LabelT, B} = cut2lines(Box),
  LB = draw_box(left, L, B),
  {LPos, Label} = label_to_right(V, LB, LabelT),
  RB = draw_box(right, R, boundbox(expand_box(LB), LPos+1)),
  BBox = boundbox(RB, length(Label)-1),
  Line = case Dir of
           right -> drawline($\\, LPos, Box#box.width, LineT);
           left  -> drawline($/, LPos, BBox#box.width, LineT)
         end,
  add2lines(Line, Label, BBox).

cut2lines(#box{draw = [Line, Label|Rest]} = Box) ->
  {Line, Label, Box#box{draw = Rest}};
cut2lines(#box{draw = []} = Box) -> {[], [], Box}.

add2lines(A, B, Box) ->
  Box#box{ draw = [A, B | Box#box.draw] }.

label_to_right(V, Box, Tail) ->
  label(V, Box#box.width, Tail).

label(V, Pos, Tail) ->
  Str = lists:flatten(io_lib:write(V)),
  HalfStr = length(Str) div 2,
  Line = expandline(Pos - HalfStr, Tail),
  {length(Line) + HalfStr, [$\s|lists:reverse(Str, Line)]}.

expand_box(Box) ->
  expand_box(Box, 1).

expand_box(Box, Width) ->
  setwidth(Box, Box#box.width + Width).

boundbox(Box, Width) ->
  setwidth(Box, max(Box#box.width, Width)).

setwidth(Box, Width) ->
  Box#box{width = Width}.

drawline(Symb, A, B, Tail) ->
  [Symb | expandline((A+B) div 2, Tail)].

expandline(Pos, Tail) ->
  spaces(Pos - length(Tail), Tail).

spaces(Left, Line) when Left > 0 ->
  spaces(Left - 1, [$\s|Line]);
spaces(_, Line) -> Line.