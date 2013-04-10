## Arrays

An array is an object with components arranged according to
a rectilinear coordinate system. One-dimensional arrays are
called *vectors*.

A *general array* may contain any Lisp object, whereas a
*specialized array* contains elements restricted to a
specified type. Vectors whose elements are restricted to
type *string-char* are called *strings*. Vectors whose
elements are restricted to type *bit* are called
*bit-vectors*.

* [HyperSpec: Arrays](http://www.lispworks.com/documentation/HyperSpec/Body/15_.htm)
* [CLtL2](http://www.cs.cmu.edu/Groups/AI/html/cltl/clm/node157.html)
* [PCL](http://www.gigamonkeys.com/book/collections.html)

## Create

m4_include(../sections/array-create.md)

## Select

m4_include(../sections/array-select.md)


### [row-major-aref] array index => element

Considers array as a vector by viewing its elements in
row-major order, and returns the element of that vector
which is referred to by the given index.

### [array-row-major-index] array subscripts\* => index

Computes the position according to the row-major ordering of
array for the element that is specified by subscripts, and
returns the offset of the element in the computed position
from the beginning of array.

### [array-dimensions] array => dimensions

Returns a list of the dimensions of array. (If array is a
vector with a fill pointer, that fill pointer is ignored.)

~~~
(array-dimensions (make-array 4))                 ;=> (4)
(array-dimensions (make-array '(2 3)))            ;=> (2 3)
(array-dimensions (make-array 4 :fill-pointer 2)) ;=> (4)
~~~

### [array-dimension] array axis-number => dimension

Returns the axis-number dimension[1] of array. (Any fill
pointer is ignored.)

~~~
(array-dimension (make-array 4) 0)      ;=> 4
(array-dimension (make-array '(2 3)) 1) ;=> 3
~~~

### [array-total-size] array => size

Returns the array total size of the array.

~~~
(array-total-size (make-array 4))                 ;=> 4
(array-total-size (make-array 4 :fill-pointer 2)) ;=> 4
(array-total-size (make-array 0))                 ;=> 0
(array-total-size (make-array '(4 2)))            ;=> 8
(array-total-size (make-array '(4 0)))            ;=> 0
(array-total-size (make-array '()))               ;=> 1
~~~

### [array-rank] array => rank

Returns the number of dimensions of array.

~~~
(array-rank (make-array '()))    ;=> 0
(array-rank (make-array 4))      ;=> 1
(array-rank (make-array '(4)))   ;=> 1
(array-rank (make-array '(2 3))) ;=> 2
~~~

### [array-displacement] array => displaced-to, displaced-index-offset

If the array is a displaced array, returns the values of the
:displaced-to and :displaced-index-offset options for the
array (see the functions make-array and adjust-array). If
the array is not a displaced array, nil and 0 are returned.

### [array-rank-limit]

Constant.

### [array-dimension-limit]

Constant.

### [array-total-size-limit]

Constant variable. The upper exclusive bound on the array
total size of an array.


m4_include(../sections/sequence-select.md)
