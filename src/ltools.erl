%%%-------------------------------------------------------------------
%%% Author  :
%%% Description :
%%%
%%% Created :
%%%-------------------------------------------------------------------
-module(ltools).
-compile([export_all, nowarn_export_all]). %% TODO: To delete after build

-include_lib("kernel/include/logger.hrl").
-include_lib("eunit/include/eunit.hrl").

%% API
%%-export([]).


%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Returns the sum of all the elements of a list.
%% @end
%%--------------------------------------------------------------------
-spec sum(List :: [number()]) -> 
    Result :: number().
sum(List) -> 
    lists:sum(List).

sum_test() -> 
    ?assertEqual(10,  sum([1, 2, 3, 4])),
    ?assertEqual(3.0, sum([1, 1.0, 2, -1])).

%%--------------------------------------------------------------------
%% @doc Returns the multiplication of all the elements of a list.
%% @end
%%--------------------------------------------------------------------
-spec mult(List :: [number()]) -> 
    Result :: number().
mult([H | Rest]) -> 
    H * mult(Rest);
mult([]) -> 
    1.

mult_test() -> 
    ?assertEqual(24,   mult([1, 2, 3, 4])),
    ?assertEqual(-2.0, mult([1, 1.0, 2, -1])).

%%--------------------------------------------------------------------
%% @doc Returns the mean of a list.
%% @end
%%--------------------------------------------------------------------
-spec mean(List :: [number()]) -> 
    Result :: number().
mean(List) -> 
    sum(List) / length(List).

mean_test() -> 
    ?assertEqual(2.5, mean([1, 2, 3, 4])),
    ?assertEqual(1.0, mean([1, 1.0, 3, -1])).

%%--------------------------------------------------------------------
%% @doc Drops the nth element of a list.
%% @end
%%--------------------------------------------------------------------
-spec drop(N :: integer(), List1 :: [term()]) -> 
    List2 :: [term()].
drop(N, [A,B,C,D|Xs]) when N >= 5 -> [A,B,C,D|drop(N-4, Xs)];
drop(4, [A,B,C,_|Xs])             -> [A,B,C  |Xs];
drop(3, [A,B,_  |Xs])             -> [A,B    |Xs];
drop(2, [A,_    |Xs])             -> [A      |Xs];
drop(1, [_      |Xs])             ->          Xs .

drop_test() -> 
    Seq9 = lists:seq(1,9),
    ?assertEqual(tl(Seq9),             drop(1, Seq9)),
    ?assertEqual([1,3,4,5,6,7,8,9],    drop(2, Seq9)),
    ?assertEqual([1,2,3,4,5,7,8,9],    drop(6, Seq9)),
    ?assertEqual(lists:droplast(Seq9), drop(length(Seq9), Seq9)),
    ?assertError(function_clause,      drop(0, Seq9)),
    ?assertError(function_clause,      drop(length(Seq9)+1, Seq9)).

%%--------------------------------------------------------------------
%% @doc Returns a list with the list positions of an element.
%% @end
%%--------------------------------------------------------------------
-spec pos(Elem :: term(), List :: [term()]) -> 
    Positions :: [integer()].
pos(Elem, List) -> 
    ElemN = lists:zip(List, 
                      lists:seq(1, length(List))),
    proplists:append_values(Elem, ElemN).

pos_test() -> 
    List1 = [a,b,c,d,e,a,a,b,z],
    % Index= 1,2,3,4,5,6,7,8,9
    ?assertEqual([1,6,7        ], pos(a, List1)),
    ?assertEqual([2,8          ], pos(b, List1)),
    ?assertEqual([3            ], pos(c, List1)),
    ?assertEqual([             ], pos(y, List1)),
    ?assertEqual([length(List1)], pos(z, List1)).

%%--------------------------------------------------------------------
%% @doc Splits a list into a list using an index list.
%% @end
%%--------------------------------------------------------------------
-spec split(Indexes :: [integer()], List :: [term()]) -> 
    ListOfLists :: [[term()]].
split(Indexes, List) -> 
    split(Indexes, List, 0).

split([I|Ix], List, P) -> 
    {Split, Rest} = lists:split(I-P, List),
    [Split | split(Ix, Rest, I)];
split(    [], Rest, _) -> [Rest];
split(     _,   [], _) -> [].

split_test() -> 
    Seq9 = lists:seq(1,9),
    ?assertError(badarg,                      split([-1], Seq9)),
    ?assertEqual([[],Seq9],                   split([0], Seq9)),
    ?assertEqual([Seq9],                      split([], Seq9)),
    ?assertEqual([[hd(Seq9)],tl(Seq9)],       split([1], Seq9)),
    ?assertEqual([[1],[2,3],[4,5],[6,7,8,9]], split([1,3,5], Seq9)),
    ?assertEqual([Seq9,[]],                   split([9], Seq9)).

%%--------------------------------------------------------------------
%% @doc Gets the list elements using a list of indexes.
%% @end
%%--------------------------------------------------------------------
-spec get(Indexes :: [integer()], List1 :: [term()]) -> 
    List2 :: [term()].
get(Indexes, List1) -> 
    NIndexes = lists:zip(lists:seq(1, length(Indexes)), 
                         Indexes),
    NElements = get(lists:keysort(2, NIndexes), List1, 1),
    [Elem || {_, Elem} <- lists:keysort(1, NElements)].

get([{N,I}|Ix], [A|Xs], I) -> [{N,A}|get(Ix, [A|Xs], I)];
get(       Ix,  [_|Xs], I) ->        get(Ix,     Xs, I+1);
get(        _,       _, _) -> [].

get_test() -> 
    Seq9 = lists:seq(1,9),
    Rnd9 = [rand:uniform(9) || _ <- Seq9],
    ?assertEqual(Seq9, get(Seq9, Seq9)),
    ?assertEqual(Rnd9, get(Rnd9, Seq9)).

%%--------------------------------------------------------------------
%% @doc Performs the replace indicated by tuple_pair on the list.
%% Replacement order is indicated by the replace list order.
%% @end
%%--------------------------------------------------------------------
-spec replace(Replace :: [{term(), term()}], List1 :: [term()]) -> 
    List2 :: [term()].
replace(Replace, List) -> 
    Fun = fun(Elem) -> proplists:get_value(Elem, Replace, Elem) end,
    lists:map(Fun, List).

replace_test() -> 
    Seq4 = lists:seq(1,4),
    ?assertEqual([1,2,3,9], replace([{4,9}], Seq4)),
    ?assertEqual([2,3,3,4], replace([{1,2},{2,3}], Seq4)),
    ?assertEqual(Seq4,      replace([{a,b}], Seq4)).

%%--------------------------------------------------------------------
%% @doc Creates a sublist of every nth elements.
%% @end
%%--------------------------------------------------------------------
-spec each(N :: integer(), List1 :: [term()], Start :: integer()) -> 
    List2 :: [term()].
each(N, [_,_,_,_,_|Xs], I) when I >= 5 -> each(N, Xs, I-5);
each(N, [_,_,_,H  |Xs], 4)             -> [H | each(N, Xs, N)];
each(N, [_,_,H    |Xs], 3)             -> [H | each(N, Xs, N)];
each(N, [_,H      |Xs], 2)             -> [H | each(N, Xs, N)];
each(N, [H        |Xs], 1)             -> [H | each(N, Xs, N)];
each(_,              _, _)             -> [].

each_test() -> 
    Seq9 = lists:seq(1,9),
    ?assertEqual(Seq9,        each(1, Seq9, 1)),
    ?assertEqual(tl(Seq9),    each(1, Seq9, 2)),
    ?assertEqual([1,3,5,7,9], each(2, Seq9, 1)),
    ?assertEqual([2,4,6,8],   each(2, Seq9, 2)),
    ?assertEqual([3,5,7,9],   each(2, Seq9, 3)),
    ?assertEqual([2,5,8],     each(3, Seq9, 2)),
    ?assertEqual([1,9],       each(8, Seq9, 1)),
    ?assertEqual([2,8],       each(6, Seq9, 2)).

