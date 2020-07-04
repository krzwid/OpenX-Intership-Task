-module(tree).
-export([makeTree/1, addLeaf/3, getSubtree/2,
  sumSubtree/2,  averageSubtree/2, medianSubtree/2,
  get_verical_lines/2]).

-export([draw/1, get_verical_lines/1]).

makeTree(Root) -> {Root, nil, nil}.

addLeaf({Root, Left, Right}, Where, Number) ->
  case Where of
    [r] ->
      case Right of
        nil -> {Root, Left, {Number, nil, nil}};
        _ -> {error,"Canot insert root in this place"}
      end;
    [l] ->
      case Left of
        nil -> {Root, {Number, nil, nil}, Right};
        _ -> {error,"Canot insert root in this place"}
      end;
    [r | Tail] ->
      case Right of
        nil -> {error,"Canot insert root in this place"};
        _ -> {Root, Left, addLeaf(Right, Tail, Number)}
      end;
    [l | Tail] ->
      case Left of
        nil -> {error,"Canot insert root in this place"};
        _ -> {Root, addLeaf(Left, Tail, Number), Right}
      end
  end.

getSubtree(Tree, []) -> Tree;
getSubtree({_, Left, Right}, [Head | Tail]) ->
  case Head of
    r ->
      case Right of
        nil -> {error,"Canot get that subtree"};
        _ ->getSubtree(Right, Tail)
      end;
    l ->
      case Left of
        nil -> {error,"Canot get that subtree"};
        _ -> getSubtree(Left, Tail)
      end
  end.

sumTree(nil) -> 0;
sumTree({Root, nil, nil}) -> Root;
sumTree({Root, Left, Right}) -> Root + sumTree(Left) + sumTree(Right).

numerOfLeafs(nil) -> 0;
numerOfLeafs({_, Left, Right}) -> 1 + numerOfLeafs(Left) + numerOfLeafs(Right).

average({Root, nil, nil}) -> Root;
average(Tree) -> sumTree(Tree) / numerOfLeafs(Tree).

averageSubtree(Tree, []) -> average(Tree);
averageSubtree(Tree, Direction) ->
  Subtree = getSubtree(Tree, Direction),
  average(Subtree).

sumSubtree(Tree, []) -> sumTree(Tree);
sumSubtree(Tree, Direction) ->
  Subtree = getSubtree(Tree, Direction),
  sumTree(Subtree).

median({Root, nil, nil}) -> [Root];
median(Tree) ->
  List = sortLeafs(Tree),
  NumberOfElements = numerOfLeafs(Tree),
  Half = NumberOfElements div 2,
  case (NumberOfElements rem 2) of
    0 -> (lists:nth(Half+1, List) + lists:nth(Half, List))/2;
    1 -> lists:nth(Half+1, List)
  end.

sortLeafs(Tree) ->
  List = elements(Tree),
  lists:sort(List).

elements(nil) -> [];
elements({Root, Left, Right}) -> elements(Left) ++ [Root]  ++ elements(Right).

medianSubtree(Tree, []) -> median(Tree);
medianSubtree(Tree, Direction) ->
  Subtree = getSubtree(Tree, Direction),
  [Median] = median(Subtree),
  Median.

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