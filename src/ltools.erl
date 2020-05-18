%%%-------------------------------------------------------------------
%%% Author  :
%%% Description :
%%%
%%% Created :
%%%-------------------------------------------------------------------
-module(ltools).

-include_lib("kernel/include/logger.hrl").
-include_lib("eunit/include/eunit.hrl").

%% API
-export([sum/1, mult/1, mean/1]).
-export([drop/2, setnth/3, pos/2]).
-export([split/2, get/2, replace/2, each/3]).
-export([randnth/1, shuffle/1, rand_scale/1]).


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
%% @doc Sets the nth element of a list to an specific term.
%% @end
%%--------------------------------------------------------------------
-spec setnth(N :: integer(), List1 :: [term()], New :: term()) -> 
    List2 :: [term()].
setnth(I, [A,B,C,D|Xs], X) when I>=5 -> [A,B,C,D|setnth(I-4, Xs, X)];
setnth(4, [A,B,C,_|Xs], X)           -> [A,B,C,X|Xs];
setnth(3, [A,B,_  |Xs], X)           -> [A,B,X  |Xs];
setnth(2, [A,_    |Xs], X)           -> [A,X    |Xs];
setnth(1, [_      |Xs], X)           -> [X      |Xs].

setnth_test() -> 
    Seq7 = lists:seq(1,7),
    ?assertError(function_clause, setnth(             0, Seq7, a)),
    ?assertEqual([a,2,3,4,5,6,7], setnth(             1, Seq7, a)),
    ?assertEqual([1,a,3,4,5,6,7], setnth(             2, Seq7, a)),
    ?assertEqual([1,2,3,4,5,6,a], setnth(  length(Seq7), Seq7, a)),
    ?assertError(function_clause, setnth(1+length(Seq7), Seq7, a)).

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

%%--------------------------------------------------------------------
%% @doc Returns a random element from a list.
%% @end
%%--------------------------------------------------------------------
-spec randnth(List :: [term()]) -> 
    Element :: term().
randnth(List) -> 
    lists:nth(rand:uniform(length(List)), List).

randnth_test() -> 
    Seq4 = lists:seq(1,4),
    BigList = [randnth(Seq4) || _ <- lists:seq(1,100)],
    ?assert(lists:member(1, BigList)),
    ?assert(lists:member(2, BigList)),
    ?assert(lists:member(3, BigList)),
    ?assert(lists:member(4, BigList)).

%%--------------------------------------------------------------------
%% @doc Returns a random element from a list.
%% @end
%%--------------------------------------------------------------------
-spec rand(List1 :: [term()], Probability :: float()) -> 
    List2 :: [term()].
rand(List, Probability) -> 
    [X || X <- List, rand:uniform() < Probability].

rand_test() -> 
    Seq9 = lists:seq(1,9),
    ?assertEqual([], rand(Seq9, 1.0) -- Seq9),
    ?assertEqual([], rand(Seq9, 0.0)).

%%--------------------------------------------------------------------
%% @doc Shuffles the elements of the list.
%% @end
%%--------------------------------------------------------------------
-spec shuffle(List1 :: [term()]) -> 
    List2 :: [term()].
shuffle(List) -> 
    [Y||{_,Y} <- lists:sort([{rand:uniform(), N} || N <- List])].

shuffle_test() -> 
    Seq9 = lists:seq(1,9),
    ?assertNotEqual(Seq9, shuffle(Seq9)).

%%--------------------------------------------------------------------
%% @doc Returns the element acording to a probability scalation.
%% Example: [{a,0.1},{b,0.2}] -> a = 0.1%, b = 0.2%, {} = 0.7%
%% @end
%%--------------------------------------------------------------------
-spec rand_scale(List :: [{X :: term(), Probability :: float()}]) -> 
    X :: term() | {}.
rand_scale(List) -> rand_scale(List, rand:uniform(), 0.0).

rand_scale([{Term,Prob}|List], R, RAcc0) when is_float(Prob) -> 
    case Prob + RAcc0 of 
        RAcc1 when RAcc1 < R -> rand_scale(List, R, RAcc1);
        _                    -> Term
    end;
rand_scale(                [], _, _) -> {};
rand_scale([{   _,Prob}|   _], _, _) -> error({badarg, Prob}).

rand_scale_test() -> 
    ?assertEqual( a, rand_scale([{a,1.0},{b,0.0}])),
    ?assertEqual( b, rand_scale([{a,0.0},{b,1.0}])),
    ?assertEqual({}, rand_scale([{a,0.0},{b,0.0}])),
    Seq1000 = lists:seq(1,1000),
    RandSel = [ltools:rand_scale([{a,0.3},{b,0.3}]) || _ <- Seq1000],
    ?assert(240 < length([ok ||  a <- RandSel])),
    ?assert(360 > length([ok ||  a <- RandSel])),
    ?assert(240 < length([ok ||  b <- RandSel])),
    ?assert(360 > length([ok ||  b <- RandSel])),
    ?assert(340 < length([ok || {} <- RandSel])),
    ?assert(460 > length([ok || {} <- RandSel])).

