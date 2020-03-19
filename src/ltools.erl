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
    ?assertEqual(10, sum([1, 2, 3, 4])),
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
    ?assertEqual(24, mult([1, 2, 3, 4])),
    ?assertEqual(-2.0, mult([1, 1.0, 2, -1])).

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
    ?assertEqual(tl(Seq9), drop(1, Seq9)),
    ?assertEqual([1,3,4,5,6,7,8,9], drop(2, Seq9)),
    ?assertEqual([1,2,3,4,5,7,8,9], drop(6, Seq9)),
    ?assertEqual(lists:droplast(Seq9), drop(length(Seq9), Seq9)),
    ?assertError(function_clause, drop(0, Seq9)),
    ?assertError(function_clause, drop(length(Seq9)+1, Seq9)).

%%--------------------------------------------------------------------
%% @doc Gets the elements from a list from a list of indexes.
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
    ?assertEqual(Seq9, each(1, Seq9, 1)),
    ?assertEqual(tl(Seq9), each(1, Seq9, 2)),
    ?assertEqual([1,3,5,7,9], each(2, Seq9, 1)),
    ?assertEqual([2,4,6,8], each(2, Seq9, 2)),
    ?assertEqual([3,5,7,9], each(2, Seq9, 3)),
    ?assertEqual([2,5,8], each(3, Seq9, 2)),
    ?assertEqual([1,9], each(8, Seq9, 1)),
    ?assertEqual([2,8], each(6, Seq9, 2)).

