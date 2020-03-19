-module(tree_test).
-include_lib("eunit/include/eunit.hrl").

makeTree_test() ->
  T1 = {5,nil,nil},
  ?assertEqual(T1, tree:makeTree(empty, root, 5)),
  T2 = {5,{3,nil,nil},nil},
  ?assertEqual(T2, tree:makeTree(T1, [l], 3)),
  T3 = {5,{3,{2,nil,nil},nil},nil},
  ?assertEqual(T3, tree:makeTree(T2, [l,l], 2)),
  T4 = {5,{3,{2,nil,nil},{5,nil,nil}},nil},
  ?assertEqual(T4, tree:makeTree(T3, [l,r], 5)),
  T5 = {5,{3,{2,nil,nil},{5,nil,nil}},{7,nil,nil}},
  ?assertEqual(T5, tree:makeTree(T4, [r], 7)),
  T6 = {5,{3,{2,nil,nil},{5,nil,nil}},{7,{1,nil,nil},nil}},
  ?assertEqual(T6, tree:makeTree(T5, [r,l], 1)),
  T7 = {5,{3,{2,nil,nil},{5,nil,nil}},{7,{1,nil,nil},{0,nil,nil}}},
  ?assertEqual(T7, tree:makeTree(T6, [r,r], 0)),
  T8 = {5,{3,{2,nil,nil},{5,nil,nil}},{7,{1,nil,nil},{0,{2,nil,nil},nil}}},
  ?assertEqual(T8, tree:makeTree(T7, [r,r,l], 2)),
  T9 = {5,{3,{2,nil,nil},{5,nil,nil}},{7,{1,nil,nil},{0,{2,nil,nil},{8,nil,nil}}}},
  ?assertEqual(T9, tree:makeTree(T8, [r,r,r], 8)),
  OurTree = {5,{3,{2,nil,nil},{5,nil,nil}},{7,{1,nil,nil},{0,{2,nil,nil},{8,nil,{5,nil,nil}}}}},
  ?assertEqual(OurTree, tree:makeTree(T9, [r,r,r,r],5)).

subTree_test() ->
  OurTree = {5,{3,{2,nil,nil},{5,nil,nil}},{7,{1,nil,nil},{0,{2,nil,nil},{8,nil,{5,nil,nil}}}}},
  ?assertEqual({7,{1,nil,nil},{0,{2,nil,nil},{8,nil,{5,nil,nil}}}}, tree:subTree(OurTree, [r])),
  ?assertEqual({3,{2,nil,nil},{5,nil,nil}}, tree:subTree(OurTree, [l])),
  ?assertEqual({5,nil,nil}, tree:subTree(OurTree, [l,r])),
  ?assertEqual({0,{2,nil,nil},{8,nil,{5,nil,nil}}}, tree:subTree(OurTree, [r,r])).

sumTree_test() ->
  T = {1, {2, {4, nil, nil}, {5, nil, nil}}, {3, {6, nil, nil}, {7, nil, nil}}},
  ?assertEqual(28, tree:sumTree(T)),
  OurTree = {5,{3,{2,nil,nil},{5,nil,nil}},{7,{1,nil,nil},{0,{2,nil,nil},{8,nil,{5,nil,nil}}}}},
  ?assertEqual(38, tree:sumTree(OurTree)),
  ?assertEqual(15, tree:sumTree(tree:subTree(OurTree, [r,r]))).

numElements_test() ->
  T = {1, {2, {4, nil, nil}, {5, nil, nil}}, {3, {6, nil, nil}, {7, nil, nil}}},
  ?assertEqual(7, tree:numElements(T)),
  OurTree = {5,{3,{2,nil,nil},{5,nil,nil}},{7,{1,nil,nil},{0,{2,nil,nil},{8,nil,{5,nil,nil}}}}},
  ?assertEqual(10, tree:numElements(OurTree)),
  ?assertEqual(4, tree:numElements(tree:subTree(OurTree, [r,r]))).

average_test() ->
  T = {1, {2, {4, nil, nil}, {5, nil, nil}}, {3, {6, nil, nil}, {7, nil, nil}}},
  ?assertEqual(4.0, tree:average(T)),
  OurTree = {5,{3,{2,nil,nil},{5,nil,nil}},{7,{1,nil,nil},{0,{2,nil,nil},{8,nil,{5,nil,nil}}}}},
  ?assertEqual(3.8, tree:average(OurTree)),
  ?assertEqual(3.75, tree:average(tree:subTree(OurTree, [r,r]))).

elements_test() ->
  T = {1, {2, {4, nil, nil}, {5, nil, nil}}, {3, {6, nil, nil}, {7, nil, nil}}},
  ?assertEqual([4,2,5,1,6,3,7], tree:elements(T)),
  OurTree = {5,{3,{2,nil,nil},{5,nil,nil}},{7,{1,nil,nil},{0,{2,nil,nil},{8,nil,{5,nil,nil}}}}},
  ?assertEqual([2,3,5,5,1,7,2,0,8,5], tree:elements(OurTree)),
  ?assertEqual([2,0,8,5], tree:elements(tree:subTree(OurTree, [r,r]))).

median_test() ->
  T = {1, {2, {4, nil, nil}, {5, nil, nil}}, {3, {6, nil, nil}, {7, nil, nil}}},
  ?assertEqual(4, tree:median(T)),
  OurTree = {5,{3,{2,nil,nil},{5,nil,nil}},{7,{1,nil,nil},{0,{2,nil,nil},{8,nil,{5,nil,nil}}}}},
  ?assertEqual(4.0, tree:median(OurTree)),
  ?assertEqual(3.5, tree:median(tree:subTree(OurTree, [r,r]))).