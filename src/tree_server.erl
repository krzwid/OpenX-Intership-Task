-module(tree_server).
-export([start/1, stop/0, init/1]).
-export([addLeaf/2, showTree/0, getSubtree/1,
  averageSubtree/1, sumSubtree/1, medianSubtree/1,
  deleteBranch/1]).

init(Root) ->
  loop(tree:makeTree(Root)).

loop(Tree) ->
  receive
    {request, Pid, addLeaf, {Where, Number} }->
      case tree:addLeaf(Tree, Where, Number) of
        Error = {error, _ } ->
          Pid ! {reply, Error},
          loop(Tree);
        NewTree ->
          Pid ! {reply, NewTree},
          loop(NewTree)
      end;
    {request, Pid, deleteBranch, Which }->
      case tree:deleteBranch(Tree, Which) of
        Error = {error, _ } ->
          Pid ! {reply, Error},
          loop(Tree);
        NewTree ->
          Pid ! {reply, NewTree},
          loop(NewTree)
      end;
    {request, Pid, showTree, {} } ->
      io:put_chars(tree:draw(Tree)),
      Pid ! {reply, ok},
      loop(Tree);
    {request, Pid, getSubtree, Direction} ->
      case tree:getSubtree(Tree, Direction) of
        Error = {error, _ } ->
          Pid ! {reply, Error},
          loop(Tree);
        Subtree ->
          Pid ! {reply, Subtree},
          loop(Tree)
      end;
    {request, Pid, averageSubtree, Direction} ->
      case tree:averageSubtree(Tree, Direction) of
        Error = {error, _ } ->
          Pid ! {reply, Error},
          loop(Tree);
        Average ->
          Pid ! {reply, Average},
          loop(Tree)
      end;
    {request, Pid, sumSubtree, Direction} ->
      case tree:sumSubtree(Tree, Direction) of
        Error = {error, _ } ->
          Pid ! {reply, Error},
          loop(Tree);
        Sum ->
          Pid ! {reply, Sum},
          loop(Tree)
      end;
    {request, Pid, medianSubtree, Direction} ->
      case tree:medianSubtree(Tree, Direction) of
        Error = {error, _ } ->
          Pid ! {reply, Error},
          loop(Tree);
        Median ->
          Pid ! {reply, Median},
          loop(Tree)
      end;
    {request, Pid, stop} ->
      Pid ! {reply, ok}
  end.

start(Root) ->
  register(treeserver, spawn (?MODULE, init, [Root])).

stop() ->
  treeserver ! {request, self(), stop},
  unregister(treeserver),
  receive
    {reply, ok} -> ok
  end.

call(Function, Arguments) ->
  treeserver ! {request, self(), Function, Arguments},
  receive
    {reply, Reply} -> Reply
  end.

addLeaf(Where, Number) ->
  call(addLeaf, {Where, Number}).

deleteBranch(Which) ->
  call(deleteBranch, Which).

showTree() ->
  call(showTree, {}).

getSubtree(Direction) ->
  call(getSubtree, Direction).

averageSubtree(Direction) ->
  call(averageSubtree, Direction).

sumSubtree(Direction) ->
  call(sumSubtree, Direction).

medianSubtree(Direction) ->
  call(medianSubtree, Direction).


