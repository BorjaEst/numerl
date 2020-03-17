%%%-------------------------------------------------------------------
%%% File    : example_SUITE.erl
%%% Author  :
%%% Description :
%%%
%%% Created :
%%%-------------------------------------------------------------------
-module(matrix).
-compile([export_all, nowarn_export_all]). %% TODO: To delete after build

-include_lib("kernel/include/logger.hrl").
-include_lib("eunit/include/eunit.hrl").

%% API
%%-export([]).


%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Creates a matrix from a list of lists.
%% @end
%%--------------------------------------------------------------------
-spec new(ListOfLists :: [[number()]]) ->
	Matrix :: ndarray:ndarray().
new(ListOfLists) -> 
	Shape = [length(ListOfLists), length(hd(ListOfLists))],
	Buffer = array:from_list(lists:append(ListOfLists)),
	ndarray:new(Shape, Buffer).









