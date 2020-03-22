%%%-------------------------------------------------------------------
%%% Author  :
%%% Description :
%%%
%%% Created :
%%%-------------------------------------------------------------------
-module(ndarray).
-compile({no_auto_import, [size/1, apply/3]}).

-include_lib("axis.hrl").
-include_lib("kernel/include/logger.hrl").
-include_lib("eunit/include/eunit.hrl").

%% API
-export([new/2, get/2, apply/3, reshape/2, reduce/1]).
-export([ndim/1, shape/1, size/1, data/1]).
-type_export([ndarray/0, shape/0, buffer/0, index/0]).

-type index()  :: [integer()] | integer() | ':'.
-type shape()  :: [integer()].
-type buffer() :: [number()].

-record(ndarray, {
  shape :: shape(),
  buffer :: buffer()
}).
-type ndarray() :: #ndarray{}.

-define(REVERS(List), lists:reverse(List)).


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
%% @doc Returns the number of axes (dimensions) of the array.
%% @end
%%--------------------------------------------------------------------
-spec ndim(NdArray :: ndarray()) ->
  NumberOfDimensions :: integer().
ndim(NdArray) -> 
  length(NdArray#ndarray.shape).

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
-spec data(NdArray :: ndarray()) ->
  Buffer :: buffer().
data(NdArray) -> 
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
%% @todo Use another atom (for ie';') to indicate all but reversed.
%% @end
%%--------------------------------------------------------------------
-spec get(Indexes :: [index()], NdArray :: ndarray()) ->
  NdArray :: ndarray().
get(Indexes, NdArray) ->
  Shape = shape(NdArray),
  BufferIndexes = lists:flatten(buffer_index(Indexes, Shape)),
  new(shape_indexing(Indexes, Shape), 
      ltools:get(BufferIndexes, data(NdArray))).

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
%% @doc Applies a function over the specified axis. The function must
%% take as input a list of elements and return a number.
%%  - fun(List :: [number()]) -> Result :: number().
%% @end
%%--------------------------------------------------------------------
-spec apply(Fun :: function(), NdArray :: ndarray(), 
            Axis :: axis()) ->
  NdArray :: ndarray().
apply(Fun, NdArray, Axis) ->
  Shape = shape(NdArray),
  Buffer = data(NdArray),
  new(ltools:setnth(Axis+1, Shape, 1),
      lists:map(Fun, vect_data(Axis, Shape, Buffer))).

apply_3D_test() -> 
  Array3D = new([4,3,2], lists:seq(1,24)), 
  SumAll = fun lists:sum/1,
  % Check the values on axis=0 are sum
  ?assertEqual(new([1,3,2], lists:seq(10,90,16)), 
               apply(SumAll, Array3D, ?AXIS_0)),
  % Check the values on axis=1 are sum
  ?assertEqual(new([4,1,2], [15,18,21,24,51,54,57,60]), 
               apply(SumAll, Array3D, ?AXIS_1)),
  % Check the values on axis=2 are sum
  ?assertEqual(new([4,3,1], lists:seq(14,36, 2)), 
               apply(SumAll, Array3D, ?AXIS_2)).

apply_4D_test() -> 
  Array4D = new([5,4,3,2], lists:seq(1,5*4*3*2)), 
  SumAll = fun lists:sum/1,
  % Check the values on axis=0 are sum
  ?assertEqual(new([1,4,3,2], lists:seq(15,590,25)), 
               apply(SumAll, Array4D, ?AXIS_0)),
  % Check the values on axis=1 are sum
  ?assertEqual(new([5,1,3,2], [34,38,42,46,50,114,118,122,126,130,
                               194,198,202,206,210,274,278,282,286,
                               290,354,358,362,366,370,434,438,442,
                               446,450]), 
               apply(SumAll, Array4D, ?AXIS_1)),
  % Check the values on axis=2 are sum
  ?assertEqual(new([5,4,1,2], [63,66,69,72,75,78,81,84,87,90,93,96,
                               99,102,105,108,111,114,117,120,243,246,
                               249,252,255,258,261,264,267,270,273,
                               276,279,282,285,288,291,294,297,300]), 
               apply(SumAll, Array4D, ?AXIS_2)),
  % Check the values on axis=3 are sum
  ?assertEqual(new([5,4,3,1], lists:seq(62,180,2)), 
               apply(SumAll, Array4D, ?AXIS_3)).

%%--------------------------------------------------------------------
%% @doc Deletes all dimensions with shape length == 1. 
%% @end
%%--------------------------------------------------------------------
-spec reduce(NdArray :: ndarray()) ->
  NdArray :: ndarray().
reduce(NdArray) ->
  NewShape = [X || X <- shape(NdArray), X /= 1],
  new(NewShape, data(NdArray)).

reduce_test() -> 
  Array4D = new([1,2,1,2], lists:seq(1,4)), 
  ?assertEqual(new([2,2], data(Array4D)), reduce(Array4D)).


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

%%--------------------------------------------------------------------
vect_data(Axis, Shape, Buffer) -> 
    {DH, _} = lists:split(Axis+1, [1 | Shape]),
    DBunch = ltools:mult(DH),
    DAxis = lists:nth(Axis+1, Shape),
    fill_bunch(DBunch, DBunch*DAxis, Buffer).

fill_bunch(_DBunch,_Size,     []) -> [];
fill_bunch( DBunch, Size, Buffer) -> 
    Lxx = [[] || _ <- lists:seq(1, DBunch)],
    {Vx , Rest} = lists:split(Size, Buffer),
    fill_bunch(Vx, {Lxx, []}) ++ fill_bunch(DBunch, Size, Rest).

fill_bunch([V | Vx], {[   Lx |Lxx],         Axx }) -> 
    fill_bunch( Vx , {        Lxx , [[V|Lx]|Axx]});
fill_bunch(     Vx , {         [] ,         Axx }) -> 
    fill_bunch( Vx , {?REVERS(Axx),          [] });
fill_bunch(     [] , {        Axx ,          [] }) -> 
    Axx.

vect_buffer3D_test() -> 
    Shape = [4,3,2],
    Buffer = lists:seq(1,24),
    ?assertEqual([[ 4, 3, 2, 1],[ 8, 7, 6, 5],[12,11,10, 9],
                  [16,15,14,13],[20,19,18,17],[24,23,22,21]], 
                 vect_data(0, Shape, Buffer)),
    ?assertEqual([[ 9, 5, 1],[10, 6, 2],[11, 7, 3],[12, 8, 4],
                  [21,17,13],[22,18,14],[23,19,15],[24,20,16]], 
                 vect_data(1, Shape, Buffer)),
    ?assertEqual([[13, 1],[14, 2],[15, 3],[16, 4],[17, 5],[18, 6],
                  [19, 7],[20, 8],[21, 9],[22,10],[23,11],[24,12]], 
                 vect_data(2, Shape, Buffer)).

vect_buffer4D_test() -> 
    Shape = [5,4,3,2],
    Buffer = lists:seq(1,ltools:mult(Shape)),
    ?assertEqual([[X+Y
                    ||X<-lists:seq(60, 0,-60)]
                    ||Y<-lists:seq( 1,60,  1)], 
                 vect_data(3, Shape, Buffer)).

