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


%%--------------------------------------------------------------------
%% @doc Returns the ndarray buffer.
%% @end
%%--------------------------------------------------------------------
-spec buffer(NdArray :: ndarray()) ->
  Buffer :: buffer().
buffer(NdArray) -> 
  NdArray#ndarray.buffer.



%%--------------------------------------------------------------------
%% @doc Reshapes an array. 
%% One shape dimension can be -1. In this case, the value is inferred 
%% from the length of the array and remaining dimensions. 
%% @end
%%--------------------------------------------------------------------
-spec reshape(NdArray :: ndarray(), Shape :: shape()) ->
  NdArray :: ndarray().
reshape(NdArray, Shape) ->
  NewShape = calc_shape(Shape, size(NdArray), []),
  NdArray#ndarray{shape = NewShape}.



%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
calc_shape([N | Rest], Size, Acc) when N > 0-> 
  calc_shape(Rest, Size div N, [N | Acc]);
calc_shape([-1 | Rest], Size, Acc) ->
  case Size div ltools:mult(Rest) of
    N when N > 1 ->  calc_shape(Rest, Size div N, [N | Acc]);
    _ -> error(badarg) 
  end; 
calc_shape([], 1, Acc) -> 
  lists:reverse(Acc);
calc_shape(_, _, _) -> 
  error(badarg).

calc_shape_test() -> 
  % If shape has all elements (no -1) must return the same shape
  Shape1 = [1, 2, 3],
  Size = ltools:mult(Shape1),
  ?assertEqual(Shape1, calc_shape(Shape1, Size, [])), 
  % If there is a -1, the remaining size must be placed in that dim
  Shape2 = [1, -1, 3],
  ?assertEqual(Shape1, calc_shape(Shape2, Size, [])),
  % A shape with more than one (-1) must rise a badarg error
  ?assertError(badarg, calc_shape([1, -1, -1], Size, [])),
  % A shape that does not fit with the size must return an error 
  ?assertError(badarg, calc_shape([2, 1, 1], Size, [])),
  ?assertError(badarg, calc_shape([2, 3, 3], Size, [])),
  ok.


