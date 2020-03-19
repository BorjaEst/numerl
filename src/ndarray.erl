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

-type shape() :: [integer()].
-type buffer() :: [number()].
-type index() :: integer() | ':'.

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
%% @doc x
%% @end
%%--------------------------------------------------------------------
-spec get(Indexes :: [index()], NdArray :: ndarray()) ->
  NdArray :: ndarray().
get(Indexes, NdArray) ->
  Shape = shape(NdArray),
  Buffer = buffer(NdArray),

  From = calc_from(Indexes, Shape),
  Untl = calc_untl(Indexes, Shape),
  Incr = calc_incr(Indexes, Shape),

%   Shape = shape_from_indexing(Indexes),
%   Buffer = get(lists:seq(From, To, Incr), buffer(NdArray)),


  new(shape_indexing(Indexes, Shape), 
      ltools:get(lists:seq(From, Untl, Incr), Buffer)).
  % ok.

get_2D_test() -> 
  Array2D = new([3,2], lists:seq(1,6)), 
  % Check 1st dimension get
  ?assertEqual(new([3,1], lists:seq(1,3,1)), get([':',0], Array2D)),
  ?assertEqual(new([3,1], lists:seq(4,6,1)), get([':',1], Array2D)),
  ?assertError(badarg, get([':',2], Array2D)),
  % Check 2nd dimension get
  ?assertEqual(new([1,2], lists:seq(1,6,3)), get([0,':'], Array2D)),
  ?assertEqual(new([1,2], lists:seq(2,6,3)), get([1,':'], Array2D)),
  ?assertEqual(new([1,2], lists:seq(3,6,3)), get([2,':'], Array2D)).




calc_incr(Ix, Sx) -> 
  [PreDim, _] = semicolon_split(Ix, Sx),
  ltools:mult(PreDim).

calc_incr_2D_test() -> 
  Shape2D = [3,2],
  ?assertEqual(1, calc_incr([  0,0], Shape2D)),
  ?assertEqual(3, calc_incr([  2,0], Shape2D)),
  ?assertEqual(4, calc_incr([  0,1], Shape2D)),
  ?assertEqual(6, calc_incr([  2,1], Shape2D)),
  ?assertEqual(1, calc_incr([0,':'], Shape2D)),
  ?assertEqual(3, calc_incr([2,':'], Shape2D)),
  ?assertEqual(1, calc_incr([':',0], Shape2D)),
  ?assertEqual(4, calc_incr([':',1], Shape2D)).

calc_incr_3D_test() -> 
  Shape3D = [4,3,2],
  ?assertEqual( 1, calc_incr([  0,0,0], Shape3D)),
  ?assertEqual( 4, calc_incr([  3,0,0], Shape3D)),
  ?assertEqual( 9, calc_incr([  0,2,0], Shape3D)),
  ?assertEqual(18, calc_incr([  1,1,1], Shape3D)),
  ?assertEqual( 1, calc_incr([0,':',0], Shape3D)),
  ?assertEqual( 3, calc_incr([2,':',0], Shape3D)),
  ?assertEqual(13, calc_incr([':',0,1], Shape3D)),
  ?assertEqual(17, calc_incr([':',1,1], Shape3D)).






semicolon_split(Indexes, Shape) -> 
  case ltools:pos(':', Indexes) of
    []    -> [[], Indexes];
    Split -> ltools:split([X-1 || X <- Split], Shape)
  end.

semicolon_split_test() ->
  ?assertEqual([[],[1,2,3,4]], semicolon_split([1,2,3,4], [1,2,3,4])),
  ?assertEqual([[1,2],[3,4]], semicolon_split([1,2,':',4], [1,2,3,4])),
  ?assertEqual([[1],[2,3,4]], semicolon_split([1,':',3,4], [1,2,3,4])).







shape_indexing([':'|Ix], [S|Shape]) -> [S|shape_indexing(Ix, Shape)];
shape_indexing([  _|Ix], [_|Shape]) -> [1|shape_indexing(Ix, Shape)];
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
calc_from([':'|Nx], [DN|DNx]) -> calc_from([0|Nx], [DN|DNx]);
calc_from([  N|Nx], [DN|DNx]) -> 1 + N + DN*(calc_from(Nx, DNx)-1);
calc_from(    [],       [])   -> 1.

calc_from_2D_test() -> 
  Shape2D = [3,2],
  ?assertEqual(1, calc_from([  0,0], Shape2D)),
  ?assertEqual(3, calc_from([  2,0], Shape2D)),
  ?assertEqual(4, calc_from([  0,1], Shape2D)),
  ?assertEqual(6, calc_from([  2,1], Shape2D)),
  ?assertEqual(1, calc_from([0,':'], Shape2D)),
  ?assertEqual(3, calc_from([2,':'], Shape2D)),
  ?assertEqual(1, calc_from([':',0], Shape2D)),
  ?assertEqual(4, calc_from([':',1], Shape2D)).

calc_from_3D_test() -> 
  Shape3D = [4,3,2],
  ?assertEqual( 1, calc_from([  0,0,0], Shape3D)),
  ?assertEqual( 4, calc_from([  3,0,0], Shape3D)),
  ?assertEqual( 9, calc_from([  0,2,0], Shape3D)),
  ?assertEqual(18, calc_from([  1,1,1], Shape3D)),
  ?assertEqual( 1, calc_from([0,':',0], Shape3D)),
  ?assertEqual( 3, calc_from([2,':',0], Shape3D)),
  ?assertEqual(13, calc_from([':',0,1], Shape3D)),
  ?assertEqual(17, calc_from([':',1,1], Shape3D)).

%%--------------------------------------------------------------------
calc_untl([':'|Nx], [DN|DNx]) -> calc_untl([DN-1|Nx], [DN|DNx]);
calc_untl([  N|Nx], [DN|DNx]) -> 1 + N + DN*(calc_untl(Nx, DNx)-1);
calc_untl(    [],       [])   -> 1.

calc_untl_2D_test() -> 
  Shape2D = [3,2],
  ?assertEqual(1, calc_untl([  0,0], Shape2D)),
  ?assertEqual(3, calc_untl([  2,0], Shape2D)),
  ?assertEqual(4, calc_untl([  0,1], Shape2D)),
  ?assertEqual(6, calc_untl([  2,1], Shape2D)),
  ?assertEqual(4, calc_untl([0,':'], Shape2D)),
  ?assertEqual(6, calc_untl([2,':'], Shape2D)),
  ?assertEqual(3, calc_untl([':',0], Shape2D)),
  ?assertEqual(6, calc_untl([':',1], Shape2D)).

calc_untl_3D_test() -> 
  Shape3D = [4,3,2],
  ?assertEqual( 1, calc_untl([  0,0,0], Shape3D)),
  ?assertEqual( 4, calc_untl([  3,0,0], Shape3D)),
  ?assertEqual( 9, calc_untl([  0,2,0], Shape3D)),
  ?assertEqual(18, calc_untl([  1,1,1], Shape3D)),
  ?assertEqual( 9, calc_untl([0,':',0], Shape3D)),
  ?assertEqual(11, calc_untl([2,':',0], Shape3D)),
  ?assertEqual(16, calc_untl([':',0,1], Shape3D)),
  ?assertEqual(20, calc_untl([':',1,1], Shape3D)).