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
    number().
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
    number().
mult([H, Rest]) -> 
    H * mult(Rest);
mult([]) -> 
    1.

mult_test() -> 
    ?assertEqual(24, mult([1, 2, 3, 4])),
    ?assertEqual(-2.0, mult([1, 1.0, 2, -1])).





