%%%-------------------------------------------------------------------
%%% Author  :
%%% Description :
%%%
%%% Created :
%%%-------------------------------------------------------------------
-module(ndarray).
-compile({no_auto_import, [size/1]}).
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
-spec new(Shape :: shape(), Array :: array:array()) ->
	NdArray :: ndarray().
new(Shape, Array) ->
  Buffer = array:resize(ltools:mult(Shape), Array),
	#ndarray{
    shape = Shape,
    buffer = Buffer}. 

new_test() ->
  Array = [[[1,2],
            [3,4]],
           [[5,6],
            [7,8]]],
  Shape = [2,2,2],
  Buffer = array:from_list(lists:flatten(Array)), 
  NdArray = new(Shape, Buffer), 
  ?assertEqual(8, size(NdArray)).

%%--------------------------------------------------------------------
%% @doc Returns the ndarray shape.
%% @end
%%--------------------------------------------------------------------
-spec shape(NdArray :: ndarray()) ->
  Shape :: shape().
shape(NdArray) -> 
  NdArray#ndarray.shape.

%%--------------------------------------------------------------------
%% @doc Returns the ndarray size.
%% @end
%%--------------------------------------------------------------------
-spec size(NdArray :: ndarray()) ->
  Size :: integer().
size(NdArray) -> 
  array:size(NdArray#ndarray.buffer).

