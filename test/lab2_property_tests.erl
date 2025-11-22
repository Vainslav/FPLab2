-module(lab2_property_tests).
-include_lib("proper/include/proper.hrl").

-export([prop_insert_identity/0, prop_delete_idempotent/0, prop_map_identity/0, rbbag/0, fun_pred/0]).

prop_insert_identity() ->
    ?FORALL({Key, Tree}, {any(), rbbag()},
            begin
                T1 = lab2:insert(Key, Tree),
                T2 = lab2:insert(Key, lab2:insert(Key, Tree)),
                T3 = lab2:delete(Key, T2),
                lab2:equals(T1, T3)
            end).

prop_delete_idempotent() ->
    ?FORALL({Key, Tree}, {integer(), rbbag()},
            begin
                T1 = lab2:insert(Key, lab2:insert(Key, Tree)),

                T2 = lab2:delete(Key, T1),
                T3 = lab2:delete(Key, T2),
                lab2:equals(Tree, T3)
            end).

prop_map_identity() ->
    ?FORALL(Tree, rbbag(),
            begin
                IdentityMap = lab2:map(fun(X) -> X end, Tree),
                lab2:equals(Tree, IdentityMap)
            end).

rbbag() ->
    ?SIZED(Size, rbbag(Size)).

rbbag(0) ->
    nil;
rbbag(Size) when Size > 0 ->
    ?LET({Key, Count, _LeftSize},
         {integer(), resize(3, pos_integer()), resize(Size div 2, non_neg_integer())},
         begin
             Keys = lists:duplicate(Count, Key),
             Tree = lists:foldl(fun(K, Acc) -> lab2:insert(K, Acc) end, nil, Keys),
             MoreKeys = [integer() || _ <- lists:seq(1, Size - 1)],
             lists:foldl(fun(K, Acc) -> lab2:insert(K, Acc) end, Tree, MoreKeys)
         end).

fun_pred() ->
    oneof([
        fun(X) -> X rem 2 =:= 0 end,
        fun(X) -> X > 0 end,
        fun(X) -> is_integer(X) end
    ]).