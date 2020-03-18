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
drop(N, [A,B,C,D|Xs]) when N >= 4 -> [A,B,C,D|drop(N-4, Xs)];
drop(3, [A,B,C,_|Xs])             -> [A,B,C  |Xs];
drop(2, [A,B,_  |Xs])             -> [A,B    |Xs];
drop(1, [A,_    |Xs])             -> [A      |Xs];
drop(0, [_      |Xs])             ->          Xs .

drop_test() -> 
    Seq9 = lists:seq(0,9),
    ?assertEqual(tl(Seq9), drop(0, Seq9)),
    ?assertEqual([0,1,3,4,5,6,7,8,9], drop(2, Seq9)),
    ?assertEqual([0,1,2,3,4,5,7,8,9], drop(6, Seq9)),
    ?assertEqual(lists:droplast(Seq9), drop(length(Seq9)-1, Seq9)),
    ?assertError(function_clause, drop(length(Seq9), Seq9)).

%%--------------------------------------------------------------------
%% @doc Creates a sublist of every nth elements.
%% @end
%%--------------------------------------------------------------------
-spec each(N :: integer(), List1 :: [term()], Start :: integer()) -> 
    List2 :: [term()].
each(N, [_,_,_,_,_|Xs], I) when I >= 4 -> each(N, Xs, I-5);
each(N, [_,_,_,H  |Xs], 3)             -> [H | each(N, Xs, N-1)];
each(N, [_,_,H    |Xs], 2)             -> [H | each(N, Xs, N-1)];
each(N, [_,H      |Xs], 1)             -> [H | each(N, Xs, N-1)];
each(N, [H        |Xs], 0)             -> [H | each(N, Xs, N-1)];
each(_,              _, _)             -> [].

each_test() -> 
    Seq9 = lists:seq(0,9),
    ?assertEqual(Seq9, each(1, Seq9, 0)),
    ?assertEqual(tl(Seq9), each(1, Seq9, 1)),
    ?assertEqual([0,2,4,6,8], each(2, Seq9, 0)),
    ?assertEqual([1,3,5,7,9], each(2, Seq9, 1)),
    ?assertEqual([2,4,6,8], each(2, Seq9, 2)),
    ?assertEqual([2,5,8], each(3, Seq9, 2)),
    ?assertEqual([0,9], each(9, Seq9, 0)).

