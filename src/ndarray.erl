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
-type_export([ndarray/0, shape/0, buffer/0, index/0]).

-type index()  :: [integer()] | integer() | ':'.
-type shape()  :: [integer()].
-type buffer() :: [number()].

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
	NdArray :: ndarray().
new(Shape, Buffer) ->
	#ndarray{
    shape = Shape,
    buffer = Buffer}. 

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
  ltools:mult(NdArray#ndarray.shape).

size_test() ->
  Array = [[[1,2],
            [3,4]],
           [[5,6],
            [7,8]]],
  Shape = [2,2,2],
  Buffer = lists:flatten(Array), 
  NdArray = new(Shape, Buffer), 
  ?assertEqual(8, size(NdArray)).

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

%%--------------------------------------------------------------------
%% @doc Gets a sub ndarray indicated by the Indexes. An index can be: 
%% An 'integer()' to specify a unique position; A ['integer()'] to 
%% specify more than one position in that dimension; The atom ':' to
%% indicate all the positions from that dimension.  
%% @end
%%--------------------------------------------------------------------
-spec get(Indexes :: [index()], NdArray :: ndarray()) ->
  NdArray :: ndarray().
get(Indexes, NdArray) ->
  Shape = shape(NdArray),
  BufferIndexes = lists:flatten(buffer_index(Indexes, Shape)),
  new(shape_indexing(Indexes, Shape), 
      ltools:get(BufferIndexes, buffer(NdArray))).

get_2D_test() -> 
  Array2D = new([3,2], lists:seq(1,6)), 
  % Check 1st dimension get
  ?assertEqual(new([3,1], lists:seq(1,3,1)), get([':',0], Array2D)),
  ?assertEqual(new([3,1], lists:seq(4,6,1)), get([':',1], Array2D)),
  ?assertEqual(new([3,1],               []), get([':',2], Array2D)),
  % Check 2nd dimension get
  ?assertEqual(new([1,2], lists:seq(1,6,3)), get([0,':'], Array2D)),
  ?assertEqual(new([1,2], lists:seq(2,6,3)), get([1,':'], Array2D)),
  ?assertEqual(new([1,2], lists:seq(3,6,3)), get([2,':'], Array2D)).

get_3D_test() -> 
    Array3D = new([4,3,2], lists:seq(1,24)), 
    % Array = [[[ 1, 2, 3, 4],[ 5, 6, 7, 8],[ 9,10,11,12]],
    %          [[13,14,15,16],[17,18,19,20],[21,22,23,24]]],
    ?assertEqual(           Array3D, get([':',':',':'], Array3D)),
    ?assertEqual(new([1,1,1], [24]), get([3,2,1], Array3D)),
    ?assertEqual(new([1,1,1], [18]), get([1,1,1], Array3D)),
    ?assertEqual(new([1,1,1], [ 1]), get([0,0,0], Array3D)),
    ?assertEqual(new([  4,1,1], [17,18,19,20]),                 
                 get([':',1,1], Array3D)),
    ?assertEqual(new([  4,    1,  2], [ 9,10,11,12,21,22,23,24]), 
                 get([':',    2,':'], Array3D)),
    ?assertEqual(new([  4,    2,  1], [ 1, 2, 3, 4, 5, 6, 7, 8]),    
                 get([':',[0,1],  0], Array3D)),
    ?assertEqual(new([  4,    2,  1], [ 1, 2, 3, 4, 9,10,11,12]),   
                 get([':',[0,2],  0], Array3D)),
    ?assertEqual(new([  4,    1,  2], [ 5, 6, 7, 8,17,18,19,20]), 
                 get([':',  [1],':'], Array3D)).

%%--------------------------------------------------------------------
%% @doc Deletes all dimensions from shape with length 1. 
%% @end
%%--------------------------------------------------------------------
-spec reduce(NdArray :: ndarray()) ->
  NdArray :: ndarray().
reduce(NdArray) ->
  ok.

reduce_test() -> 
  Array4D = new([1,2,1,2], lists:seq(1,4)), 
  ?assertEqual(new([2,2], buffer(Array4D)), reduce(Array4D)).


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
  ?assertError(badarg, calc_shape([2, 3, 3], Size, [])).

%%--------------------------------------------------------------------
shape_indexing([  L|Ix], [_|Shape]) when is_list(L) -> 
                               [length(L)|shape_indexing(Ix, Shape)];
shape_indexing([  I|Ix], [_|Shape]) when is_integer(I) -> 
                                       [1|shape_indexing(Ix, Shape)];
shape_indexing([':'|Ix], [S|Shape]) -> [S|shape_indexing(Ix, Shape)];
shape_indexing(      [],        []) -> [].

shape_indexing_test() -> 
  Shape = [1,2,3,4,5],
  Indx1 = [':',1,':',2,4],
  ?assertEqual([1,1,3,1,1], shape_indexing(Indx1, Shape)),
  Indx2 = [':',':',':',':',4],
  ?assertEqual([1,2,3,4,1], shape_indexing(Indx2, Shape)),
  Indx3 = [1,1,1,':',':'],
  ?assertEqual([1,1,1,4,5], shape_indexing(Indx3, Shape)).

%%--------------------------------------------------------------------
buffer_index(Ix, Dx) -> 
    buffer_index(lists:reverse(Ix), lists:reverse(Dx), 0).

buffer_index([':'|Ix], [D|Dx], Acc) -> 
    I = lists:seq(0,D-1,1),
    buffer_index([I|Ix], [D|Dx], Acc);
buffer_index([  I|Ix], [D|Dx], Acc) when is_list(I) -> 
    [buffer_index(Ix, Dx, Acc*D+X) || X <-I];
buffer_index([  I|Ix], [D|Dx], Acc) -> 
    [buffer_index(Ix, Dx, Acc*D+I)];
buffer_index(      [],     [], Acc) ->  
    Acc + 1.

buffer_index_test() -> 
    Dim = [4,3,2],
    Array = [[[ 1, 2, 3, 4],[ 5, 6, 7, 8],[ 9,10,11,12]],
             [[13,14,15,16],[17,18,19,20],[21,22,23,24]]],
    ?assertEqual(Array, buffer_index([':',':',':'], Dim)),
    ?assertEqual([[[24]]], buffer_index([  3,  2,  1], Dim)),
    ?assertEqual([[[18]]], buffer_index([  1,  1,  1], Dim)),
    ?assertEqual([[[ 1]]], buffer_index([  0,  0,  0], Dim)),
    ?assertEqual([[[17,18,19,20]]],                 
                 buffer_index([':',   1,   1], Dim)),
    ?assertEqual([[[ 9,10,11,12]],[[21,22,23,24]]], 
                 buffer_index([':',   2, ':'], Dim)),
    ?assertEqual([[[ 1, 2, 3, 4],[ 5, 6, 7, 8]]],   
                 buffer_index([':',[0,1],  0], Dim)),
    ?assertEqual([[[ 1, 2, 3, 4],[ 9,10,11,12]]],   
                 buffer_index([':',[0,2],  0], Dim)),
    ?assertEqual([[[ 5, 6, 7, 8]],[[17,18,19,20]]], 
                 buffer_index([':',  [1],':'], Dim)).
