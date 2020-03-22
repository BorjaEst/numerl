%%%-------------------------------------------------------------------
%%% Author  :
%%% Description :
%%%
%%% Created :
%%%-------------------------------------------------------------------
-module(numerl).

-include_lib("axis.hrl").
-include_lib("kernel/include/logger.hrl").
-include_lib("eunit/include/eunit.hrl").

%% API
-export([mean/1, mean/2]).


%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Computes the arithmetic mean along the specified axis.
%% Returns the average of the array elements. The average is taken 
%% over the flattened array by default, otherwise over the specified
%% axis. 
%% @end
%%--------------------------------------------------------------------
-spec mean(NdArray :: ndarray:ndarray()) -> 
    Mean :: float().
mean(NdArray) -> 
    ltools:mean(ndarray:data(NdArray)).

-spec mean(NdArray :: ndarray:ndarray(), Axis :: integer()) -> 
    NdArray :: ndarray:ndarray().
mean(NdArray, Axis) -> 
    MeanF = fun ltools:mean/1,
    ndarray:apply(MeanF, NdArray, Axis).

mean_test() -> 
    NdArray = ndarray:new([4,2], [2,4,2,4,4,4,2,2]),
    % Mean over flattened array
    ?assertEqual(3.0, mean(NdArray)),
    % Mean over an specified axis
    ?assertEqual(ndarray:new([1,2], [3.0, 3.0]), 
                 mean(NdArray, ?AXIS_0)),
    ?assertEqual(ndarray:new([4,1], [3.0,4.0,2.0,3.0]), 
                 mean(NdArray, ?AXIS_1)),
    % Averaging over every n elements of a vector 
    Vector = ndarray:new([9], [1,2,3,1,2,3,1,2,3]),
    ?assertEqual(ndarray:new([1,3], [2.0,2.0,2.0]),
                 mean(ndarray:reshape(Vector, [-1,3]), ?AXIS_0)),
    ?assertEqual(ndarray:new([3,1], [1.0,2.0,3.0]),
                 mean(ndarray:reshape(Vector, [-1,3]), ?AXIS_1)).

