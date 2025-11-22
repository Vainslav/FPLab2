-module(lab2).

-include_lib("lab2.hrl").

-export([new/0, insert/2, equals/2, delete/2, foldl/3, foldr/3, filter/2, map/2]).

-spec new() -> rbbag().
new() ->
    nil.

-spec insert(any(), rbbag()) -> rbbag().
insert(Key, Tree) ->
    make_black(ins(Key, Tree)).

-spec delete(any(), rbbag()) -> rbbag().
delete(Key, Tree) ->
    NewTree = del(Key, Tree),
    make_black(NewTree).

-spec del(any(), rbbag()) -> rbbag().
del(_Key, nil) ->
    nil;
del(Key,
    #rbbag{data = {C, K},
           left = L,
           right = R} =
        Node) ->
    case compare(Key, K) of
        e when C > 1 ->
            Node#rbbag{data = {C - 1, K}};
        e ->
            case {L, R} of
                {nil, nil} ->
                    nil;
                {nil, _} ->
                    R;
                {_, nil} ->
                    L;
                _ ->
                    {MinCount, MinKey} = get_min(R),
                    NewR =
                        lists:foldl(fun(_, Acc) -> del(MinKey, Acc) end, R, lists:seq(1, MinCount)),
                    Node#rbbag{data = {MinCount, MinKey}, right = NewR}
            end;
        l ->
            NewL = del(Key, L),
            update_node(Node#rbbag{left = NewL});
        g ->
            NewR = del(Key, R),
            update_node(Node#rbbag{right = NewR})
    end.

-spec get_min(rbbag()) -> {integer(), any()}.
get_min(#rbbag{left = nil, data = Data}) ->
    Data;
get_min(#rbbag{left = L}) ->
    get_min(L).

-spec update_node(rbbag()) -> rbbag().
update_node(#rbbag{left = nil, right = nil} = Node) ->
    Node;
update_node(#rbbag{left = L, right = R} = Node) ->
    case {is_red(L), is_red(R)} of
        {true, _} ->
            balance(Node);
        {_, true} ->
            balance(Node);
        _ ->
            Node#rbbag{color = black}
    end.

-spec is_red(rbbag()) -> boolean().
is_red(#rbbag{color = red}) ->
    true;
is_red(_) ->
    false.

-spec make_black(rbbag()) -> rbbag().
make_black(#rbbag{color = red} = Node) ->
    Node#rbbag{color = black};
make_black(A) ->
    A.

-spec ins(any(), rbbag()) -> rbbag().
ins(Key, nil) ->
    #rbbag{color = red,
           data = {1, Key},
           left = nil,
           right = nil};
ins(Key,
    #rbbag{color = _Color,
           data = {C, K},
           left = L,
           right = R} =
        Node) ->
    case compare(Key, K) of
        e ->
            Node#rbbag{data = {C + 1, K}};
        l ->
            balance(Node#rbbag{left = ins(Key, L)});
        g ->
            balance(Node#rbbag{right = ins(Key, R)})
    end.

-spec balance(rbbag()) -> rbbag().
balance(#rbbag{color = black,
               data = Z,
               left =
                   #rbbag{color = red,
                          data = Y,
                          left =
                              #rbbag{color = red,
                                     data = X,
                                     left = A,
                                     right = B},
                          right = C},
               right = D}) ->
    #rbbag{color = red,
           data = Y,
           left =
               #rbbag{color = black,
                      data = X,
                      left = A,
                      right = B},
           right =
               #rbbag{color = black,
                      data = Z,
                      left = C,
                      right = D}};
balance(#rbbag{color = black,
               data = Z,
               left =
                   #rbbag{color = red,
                          data = X,
                          left = A,
                          right =
                              #rbbag{color = red,
                                     data = Y,
                                     left = B,
                                     right = C}},
               right = D}) ->
    #rbbag{color = red,
           data = Y,
           left =
               #rbbag{color = black,
                      data = X,
                      left = A,
                      right = B},
           right =
               #rbbag{color = black,
                      data = Z,
                      left = C,
                      right = D}};
balance(#rbbag{color = black,
               data = X,
               left = A,
               right =
                   #rbbag{color = red,
                          data = Y,
                          left = B,
                          right =
                              #rbbag{color = red,
                                     data = Z,
                                     left = C,
                                     right = D}}}) ->
    #rbbag{color = red,
           data = Y,
           left =
               #rbbag{color = black,
                      data = X,
                      left = A,
                      right = B},
           right =
               #rbbag{color = black,
                      data = Z,
                      left = C,
                      right = D}};
balance(#rbbag{color = black,
               data = X,
               left = A,
               right =
                   #rbbag{color = red,
                          data = Z,
                          left =
                              #rbbag{color = red,
                                     data = Y,
                                     left = B,
                                     right = C},
                          right = D}}) ->
    #rbbag{color = red,
           data = Y,
           left =
               #rbbag{color = black,
                      data = X,
                      left = A,
                      right = B},
           right =
               #rbbag{color = black,
                      data = Z,
                      left = C,
                      right = D}};
balance(A) ->
    A.

-spec compare(any(), any()) -> l | e | g.
compare(A, B) ->
    if A > B ->
           g;
       A < B ->
           l;
       A =:= B ->
           e
    end.

-spec equals(rbbag(), rbbag()) -> boolean().
equals(Tree1, Tree2) ->
    Map1 = count_values(Tree1, #{}),
    Map2 = count_values(Tree2, #{}),
    Map1 =:= Map2.

-spec count_values(rbbag(), map()) -> map().
count_values(nil, Acc) ->
    Acc;
count_values(#rbbag{data = {Count, Value},
                    left = Left,
                    right = Right},
             Acc) ->
    NewAcc =
        case Acc of
            #{Value := OldCount} ->
                Acc#{Value := OldCount + Count};
            _ ->
                Acc#{Value => Count}
        end,
    count_values(Left, count_values(Right, NewAcc)).

-spec foldl(fun((any(), T) -> T), T, rbbag()) -> T.
foldl(_Fun, Acc, nil) ->
    Acc;
foldl(Fun,
      Acc,
      #rbbag{data = {Count, Key},
             left = L,
             right = R}) ->
    NewAcc1 = foldl(Fun, Acc, L),
    NewAcc2 = lists:foldl(fun(_, A) -> Fun(Key, A) end, NewAcc1, lists:seq(1, Count)),
    foldl(Fun, NewAcc2, R).

-spec foldr(fun((any(), T) -> T), T, rbbag()) -> T.
foldr(_Fun, Acc, nil) ->
    Acc;
foldr(Fun,
      Acc,
      #rbbag{data = {Count, Key},
             left = L,
             right = R}) ->
    NewAcc1 = foldr(Fun, Acc, R),
    NewAcc2 = lists:foldr(fun(_, A) -> Fun(Key, A) end, NewAcc1, lists:seq(1, Count)),
    foldr(Fun, NewAcc2, L).

-spec filter(fun((any()) -> boolean()), rbbag()) -> rbbag().
filter(Pred, Tree) ->
    foldl(fun(Key, Acc) ->
             case Pred(Key) of
                 true ->
                     insert(Key, Acc);
                 false ->
                     Acc
             end
          end,
          new(),
          Tree).

-spec map(fun((any()) -> any()), rbbag()) -> rbbag().
map(Fun, Tree) ->
    foldl(fun(Key, Acc) ->
             NewKey = Fun(Key),
             insert(NewKey, Acc)
          end,
          new(),
          Tree).
