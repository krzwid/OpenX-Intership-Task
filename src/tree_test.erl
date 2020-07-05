-module(tree_test).
-include_lib("eunit/include/eunit.hrl").

makeTree_test_() ->
  [
    {"returns new tree wit given value in the root",
      fun() ->
        T1 = {4,nil,nil},
        ?_assertEqual(T1, tree:makeTree(4))
      end()
    }
  ].

addLeaf_test_() ->
  [
    {"add leaf to the given tree in the proper place with proper value",
      fun() ->
        T1 = {4,nil,nil},
        T2 = {4,nil,{6,nil,nil}},
        ?_assertEqual(T2, tree:addLeaf(T1, [r], 6))
      end()
    },
    {"add leaf to the given tree in the proper place with proper value",
      fun() ->
        T2 = {4,nil,{6,nil,nil}},
        T3 = {4,{2,nil,nil},{6,nil,nil}},
        ?_assertEqual(T3, tree:addLeaf(T2, [l], 2))
      end()
    },
    {"add leaf to the given tree in the proper place with proper value",
      fun() ->
        T3 = {4,{2,nil,nil},{6,nil,nil}},
        T4 = {4,{2,{1,nil,nil},nil},{6,nil,nil}},
        ?_assertEqual(T4, tree:addLeaf(T3, [l,l], 1))
      end()
    },
    {"add leaf to the given tree in the proper place with proper value",
      fun() ->
        T4 = {4,{2,{1,nil,nil},nil},{6,nil,nil}},
        T5 = {4,{2,{1,nil,nil},{3,nil,nil}},{6,nil,nil}},
        ?_assertEqual(T5, tree:addLeaf(T4, [l,r], 3))
      end()
    },
    {"add leaf to the given tree in the proper place with proper value",
      fun() ->
        T5 = {4,{2,{1,nil,nil},{3,nil,nil}},{6,nil,nil}},
        T6 = {4,{2,{1,nil,nil},{3,nil,nil}},{6,{5,nil,nil},nil}},
        ?_assertEqual(T6, tree:addLeaf(T5, [r,l], 5))
      end()
    },
    {"add leaf to the given tree in the proper place with proper value",
      fun() ->
        T6 = {4,{2,{1,nil,nil},{3,nil,nil}},{6,{5,nil,nil},nil}},
        T7 = {4,{2,{1,nil,nil},{3,nil,nil}},{6,{5,nil,nil},{7,nil,nil}}},
        ?_assertEqual(T7, tree:addLeaf(T6, [r,r], 7))
      end()
    },
    {"returns error as there is another root",
      fun() ->
        T7 = {4,{2,{1,nil,nil},{3,nil,nil}},{6,{5,nil,nil},{7,nil,nil}}},
        Error = {error,"Cannot insert root in this place"},
        ?_assertEqual(Error, tree:addLeaf(T7, [r], 10))
      end()
    }
%%    {"returns error as there is another root",
%%      fun() ->
%%        T7 = {4,{2,{1,nil,nil},{3,nil,nil}},{6,{5,nil,nil},{7,nil,nil}}},
%%        Error = {error,"Cannot insert root in this place"},
%%        ?_assertEqual(Error, tree:addLeaf(T7, [r,l], 10))
%%      end()
%%    }
  ].

getSubtree_test_() ->
  [
    {"returns subtree",
      fun() ->
        T7 = {4,{2,{1,nil,nil},{3,nil,nil}},{6,{5,nil,nil},{7,nil,nil}}},
        St1 = {2,{1,nil,nil},{3,nil,nil}},
        ?_assertEqual(St1, tree:getSubtree(T7, [l]))
      end()
    },
    {"returns subtree",
      fun() ->
        T7 = {4,{2,{1,nil,nil},{3,nil,nil}},{6,{5,nil,nil},{7,nil,nil}}},
        St2 = {5,nil,nil},
        ?_assertEqual(St2, tree:getSubtree(T7, [r,l]))
      end()
    },
    {"returns error because there is no such subtree",
      fun() ->
        T7 = {4,{2,{1,nil,nil},{3,nil,nil}},{6,{5,nil,nil},{7,nil,nil}}},
        Error = {error,"Cannot get that subtree"},
        ?_assertEqual(Error, tree:getSubtree(T7, [r,l,r]))
      end()
    }
  ].

sumSubtree_test_() ->
  [
    {"returns proper value",
      fun() ->
        T7 = {4,{2,{1,nil,nil},{3,nil,nil}},{6,{5,nil,nil},{7,nil,nil}}},
%%        St1 = {2,{1,nil,nil},{3,nil,nil}},
        Sum = 6,
        ?_assertEqual(Sum, tree:sumSubtree(T7, [l]))
      end()
    },
    {"returns proper value",
      fun() ->
        T7 = {4,{2,{1,nil,nil},{3,nil,nil}},{6,{5,nil,nil},{7,nil,nil}}},
%%        St2 = {5,nil,nil},
        Sum = 5,
        ?_assertEqual(Sum, tree:sumSubtree(T7, [r,l]))
      end()
    },
    {"returns error because there is no such subtree",
      fun() ->
        T7 = {4,{2,{1,nil,nil},{3,nil,nil}},{6,{5,nil,nil},{7,nil,nil}}},
        Error = {error,"Cannot get that subtree"},
        ?_assertEqual(Error, tree:sumSubtree(T7, [r,l,r]))
      end()
    }
  ].

averageSubtree_test_() ->
  [
    {"returns proper value",
      fun() ->
        T7 = {4,{2,{1,nil,nil},{3,nil,nil}},{6,{5,nil,nil},{7,nil,nil}}},
%%        St1 = {2,{1,nil,nil},{3,nil,nil}},
        Average = 2.0,
        ?_assertEqual(Average, tree:averageSubtree(T7, [l]))
      end()
    },
    {"returns proper value",
      fun() ->
        T7 = {4,{2,{1,nil,nil},{3,nil,nil}},{6,{5,nil,nil},{7,nil,nil}}},
%%        St2 = {5,nil,nil},
        Average = 5,
        ?_assertEqual(Average, tree:averageSubtree(T7, [r,l]))
      end()
    },
    {"returns error because there is no such subtree",
      fun() ->
        T7 = {4,{2,{1,nil,nil},{3,nil,nil}},{6,{5,nil,nil},{7,nil,nil}}},
        Error = {error,"Cannot get that subtree"},
        ?_assertEqual(Error, tree:averageSubtree(T7, [r,l,r]))
      end()
    }
  ].

medianSubtree_test_() ->
  [
    {"returns proper value",
      fun() ->
        T7 = {4,{2,{1,nil,nil},{3,nil,nil}},{6,{5,nil,nil},{7,nil,nil}}},
%%        St1 = {2,{1,nil,nil},{3,nil,nil}},
        Median = 2,
        ?_assertEqual(Median, tree:medianSubtree(T7, [l]))
      end()
    },
    {"returns proper value",
      fun() ->
        T7 = {4,{2,{1,nil,nil},{3,nil,nil}},{6,{5,nil,nil},{7,nil,nil}}},
%%        St2 = {5,nil,nil},
        Median = 5,
        ?_assertEqual(Median, tree:medianSubtree(T7, [r,l]))
      end()
    },
    {"returns error because there is no such subtree",
      fun() ->
        T7 = {4,{2,{1,nil,nil},{3,nil,nil}},{6,{5,nil,nil},{7,nil,nil}}},
        Error = {error,"Cannot get that subtree"},
        ?_assertEqual(Error, tree:medianSubtree(T7, [r,l,r]))
      end()
    }
  ].