# numerl
Numeric Erlang is the fundamental package needed for scientific computing with Erlang.

> An OTP library

It provides:
- A powerful N-dimensional array module
- Useful linear algebra (In development)


## Installation
Simply run:
```sh
$ rebar3 compile
```

Use it in your project adding it as a dependency in your rebar.conf.
```erlang
{deps, 
    [
        {enn, {git, "https://github.com/BorjaEst/numerl.git", {tag, "<version>"}}}
    ]}.
```

>You can find more information about dependencies in [rebar3 - dependencies](https://www.rebar3.org/docs/dependencies). 


## Usage
This library is based on the popular [NUmPy](https://numpy.org/).
The idea is to share a similar syntax (always it is possible) see [quickstart](https://numpy.org/devdocs/user/quickstart.html) and the following comments:
- Corrdinates references are shared: numpy[1 ,2, 1] means numerl[1, 2, 1]. See 'ndarray:get/2' to know how to get the ndarray elements.
- You can use the atom ':' to reference a full dimension when using 'ndarray:get/2' to obtain sub-ndarrays.
- You can index with lists when using 'ndarray:get/2' to get more than one element in a dimension.

Note the following differences:
- Functions are called by Erlang syntax, so instead of running 'a.shape' you should run `numerl:shape(a)`.
- To access the indexes you must use 'ndarray:get/2'. For example to get a sub-ndarray of a 3D ndarray you can use `numerl:get([':',':',[0,1]],NdArray)`.
- Because Erlang is dynamically typed, you should not specify the type of the elements inside the array. 


## Examples
First thing you need is an ndarray. You can create it this way:
```erlang
1> Shape   = [4,3,2].
2> Buffer  = lists:seq(1,24).
3> NdArray = ndarray:new(Shape, Buffer).
{ndarray,[4,3,2],
         [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24]}
```
Where 'Shape' are the dimensions of the ndarray and 'Buffer' the 
values of the array. This would represent the following ndarray
in numpy:
```python
import numpy as np
ndarray = np.array([[[ 1, 5, 9],
                     [ 2, 6,10],
                     [ 3, 7,11],
                     [ 4, 8,12]],

                    [[13,17,21],
                     [14,18,22],
                     [15,19,23],
                     [16,20,24]]])
```

Now you can use the functions inside numerl to operate this ndarray. 
For example, you can calculate the mean over the axis 0 by running:
```erlang
4> Mean = numerl:mean(NdArray, Axis=0).
{ndarray,[1,3,2],[2.5,6.5,10.5,14.5,18.5,22.5]}
```

As you can see, after applying an operation over a dimension, it reduces the size of that dimension to '1'. You can compress all simple dimensions 
by running 'ndarray:reduce/1':
4>numerl:mean(NdArray, 0).
{ndarray,[1,3,2],[2.5,6.5,10.5,14.5,18.5,22.5]}
```erlang
5> ndarray:reduce(Mean).
{ndarray,[3,2],[2.5,6.5,10.5,14.5,18.5,22.5]}
```

And that is all, you can find all functions inside the module together with the documentation.


## Call for Contributions
numerl appreciates help from a wide range of different backgrounds.
Small improvements or fixes are always appreciated.
If you are considering larger contributions outside the traditional coding work, please contact me.

### TODO:
- Create a transpose function for n-dimensions array in ndarray module.
- Measure speed against other libraries:
  - When interactive mode
  - When compiled (HiPe) mode
- Add your needed functions into the numerl module.
- Tools to speed up integrating C/C++ code.

## License
This software is under [Apache-2.0](https://www.apache.org/licenses/LICENSE-2.0) license.