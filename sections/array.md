## Array

An array is an object with components arranged according to
a rectilinear coordinate system. One-dimensional arrays are
called *vectors*.

A *general array* may contain any Lisp object, whereas a
*specialized array* contains elements restricted to a
specified type. Vectors whose elements are restricted to
type *string-char* are called *strings*. Vectors whose
elements are restricted to type *bit* are called
*bit-vectors*.

* [HyperSpec: Arrays Dictionary](http://www.lispworks.com/documentation/HyperSpec/Body/c_arrays.htm)
* [CLtL2](http://www.cs.cmu.edu/Groups/AI/html/cltl/clm/node157.html)
* [PCL](http://www.gigamonkeys.com/book/collections.html)

### [aref] array &rest subscripts => element

Access array elements.
Related: `svref` ---Access index of a simple vector.

~~~
(setf a (make-array '(2 2) :initial-contents '((1 2) (3 4))))
(aref a 1 1) ;=> 4
(setf (aref a 0 1) 99) ; a => #2A((1 99) (3 4))
~~~

### [alexandria:copy-array] array &key element-type fill-pointer adjustable => new-array

Returns an undisplaced copy of *array*, with same
fill-pointer and adjustability as the original.
