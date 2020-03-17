%%%-------------------------------------------------------------------
%%% File    : example_SUITE.erl
%%% Author  :
%%% Description :
%%%
%%% Created :
%%%-------------------------------------------------------------------
-module(ndarray).
-compile([export_all, nowarn_export_all]). %% TODO: To delete after build

-include_lib("kernel/include/logger.hrl").
-include_lib("eunit/include/eunit.hrl").

%% API
%%-export([]).
-type_export([ndarray/0]).

-type shape() :: [integer()].
-type buffer() :: array:array().

-record(ndarray, {
  shape :: shape(),
  buffer :: buffer()
}).
-type ndarray() :: #ndarray{}.


%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Creates a multidimensional array from a list of lists.
%% @end
%%--------------------------------------------------------------------
-spec new(Shape :: shape(), Buffer :: buffer()) ->
	NDArray :: ndarray().
new(Shape, Buffer) ->
	#ndarray{
    shape = Shape,
    buffer = Buffer}.








