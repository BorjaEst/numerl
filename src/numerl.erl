%%%-------------------------------------------------------------------
%%% Author  :
%%% Description :
%%%
%%% Created :
%%%-------------------------------------------------------------------
-module(numerl).
-compile([export_all, nowarn_export_all]). %% TODO: To delete after build

-include_lib("axis.hrl").
-include_lib("kernel/include/logger.hrl").
-include_lib("eunit/include/eunit.hrl").

%% API
% -export([]).
% -type_export([]).


%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc 
%% @end
%%--------------------------------------------------------------------
-spec mean(NdArray :: ndarray:ndarray()) -> 
    Mean :: float().
mean(NdArray) -> 
    ok.

-spec mean(NdArray :: ndarray:ndarray(), Axis :: integer()) -> 
    NdArray :: ndarray:ndarray().
mean(NdArray, 0 = Axis) -> 


    Shape = ltools:drop(Axis, ndarray:shape(NdArray)),
    Buffer = todo,
    ndarray:new(Shape, Buffer).


mean_test() -> 
    Buffer = array:from_list([2,4,2,4,4,4,2,2]),
    Shape = [4, 2],
    NdArray = ndarray:new(Shape, Buffer),
    ?assertEqual(3.0, mean(NdArray)),
    ?assertEqual(array:from_list([12.0, 12.0]), 
                 ndarray:buffer(mean(NdArray, ?AXIS_0))),
    ?assertEqual(array:from_list([3.0, 4.0, 2.0, 3.0]), 
                 ndarray:buffer(mean(NdArray, ?AXIS_1))),
    ?assertError(badarg, mean(NdArray, ?AXIS_2)),

    % Buffer = array:from_list([1,2,3,1,2,3,1,2,3]),
    % Shape = [array:size(Buffer)],
    % NdArray = ndarray:new(Shape, Buffer),
    % % Averaging over every n elements of a ndarray
    % AxisMean = mean(ndarray:reshape(NdArray, [-1, 3]), ?AXIS_1),
    % ?assertEqual([2,2,2], ndarray:buffer(AxisMean)).

    ok.







