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

### [make-array] dims &key type initial-element initial-contents adjustable fill-pointer displaced-to displaced-index-offset => new-array

~~~
(make-array '(2 3)) ;=> #2A((0 0 0) (0 0 0))
(make-array 5 :fill-pointer 0 :adjustable t :element-type 'character) ;=> ""
~~~

### [adjust-array] array new-dims &key type initial-element initial-contents fill-pointer displaced-to displaced-index-offset => adjusted-array

### [alexandria:copy-array] array &key element-type fill-pointer adjustable => new-array

Returns an undisplaced copy of *array*, with same
fill-pointer and adjustability as the original.

### [aref] array &rest subscripts => element

Access array elements.
Related: [svref] ---Access index of a simple vector.

~~~
(setf a (make-array '(2 2) :initial-contents '((1 2) (3 4))))
(aref a 1 1) ;=> 4
(setf (aref a 0 1) 99) ; a => #2A((1 99) (3 4))
~~~

### [row-major-aref]

### [array-row-major-index]

### [array-dimensions]

### [array-dimension]

### [array-total-size]

### [array-rank]

### [array-displacement]

### [array-rank-limit]

### [array-dimension-limit]

### [array-total-size-limit]
