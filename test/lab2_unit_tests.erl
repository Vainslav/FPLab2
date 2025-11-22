-module(lab2_unit_tests).

-include_lib("lab2.hrl").

-include_lib("eunit/include/eunit.hrl").

unit_test_() ->
    [
        {"New creates empty tree", fun test_new/0},
        {"lab2:insert creates single element", fun insert_single/0},
        {"lab2:insert same key increases count", fun insert_duplicate/0},
        {"lab2:delete reduces count", fun delete_reduce/0},
        {"lab2:delete removes element", fun delete_remove/0},
        {"Equals works for same trees", fun equals_same/0},
        {"Foldl sums correctly", fun foldl/0},
        {"Filter works", fun filter/0},
        {"Map transforms correctly", fun map/0}
    ].

test_new() ->
    Empty = lab2:new(),
    Empty =:= nil.

insert_single() ->
    Tree = lab2:insert(5, lab2:new()),
    Map = count_values(Tree, #{}),
    maps:get(5, Map) =:= 1.

insert_duplicate() ->
    Tree = lab2:insert(5, lab2:insert(5, lab2:new())),
    Map = count_values(Tree, #{}),
    maps:get(5, Map) =:= 2.

delete_reduce() ->
    Tree1 = lab2:insert(5, lab2:insert(5, lab2:new())),
    Tree2 = lab2:delete(5, Tree1),
    Map = count_values(Tree2, #{}),
    io:write(Map),
    maps:get(5, Map) =:= 1.

delete_remove() ->
    Tree1 = lab2:insert(5, lab2:new()),
    Tree2 = lab2:delete(5, Tree1),
    Tree2 =:= nil.

equals_same() ->
    Tree1 = lab2:insert(5, lab2:insert(3, lab2:new())),
    Tree2 = lab2:insert(3, lab2:insert(5, lab2:new())),
    lab2:equals(Tree1, Tree2).

foldl() ->
    Tree = lab2:insert(1, lab2:insert(2, lab2:insert(3, lab2:new()))),
    Sum = lab2:foldl(fun(X, Acc) -> Acc + X end, 0, Tree),
    Sum =:= 6.

filter() ->
    Tree = lab2:insert(1, lab2:insert(2, lab2:insert(3, lab2:insert(4, lab2:new())))),
    EvenFilter = fun(X) -> X rem 2 =:= 0 end,
    Filtered = lab2:filter(EvenFilter, Tree),
    Map = count_values(Filtered, #{}),
    maps:keys(Map) =:= [2, 4].

map() ->
    Tree = lab2:insert(1, lab2:insert(2, lab2:new())),
    Doubled = lab2:map(fun(X) -> X * 2 end, Tree),
    Map = count_values(Doubled, #{}),
    maps:keys(Map) =:= [2, 4],
    maps:get(2, Map) =:= 1,
    maps:get(4, Map) =:= 1.


count_values(nil, Acc) ->
    Acc;
count_values(#rbbag{data = {Count, Value}, left = Left, right = Right}, Acc) ->
    NewAcc = case Acc of
        #{Value := OldCount} -> Acc#{Value := OldCount + Count};
        _ -> Acc#{Value => Count}
    end,
    count_values(Left, count_values(Right, NewAcc)).