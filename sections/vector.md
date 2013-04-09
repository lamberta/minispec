## Vectors

A vector is a basic integer-indexed collection, a
one-dimensional [array]. Vectors and lists are collectively
considered to be *sequences*.

* [CLtL2: Vectors](http://www.cs.cmu.edu/Groups/AI/html/cltl/clm/node30.html)
* [PCL: Vectors](http://www.gigamonkeys.com/book/collections.html#vectors)

### [vector] &rest objects\* => vector

### [svref] simple-vector index => element

Accesses the element of *simple-vector* specified by *index*.

### [vector-push] new-element vector => index

~~~
(setf v (make-array 2 :fill-pointer 0)) ;=> #()
(vector-push 'a v)                      ;=> 0   [v => #(A)]
(vector-push 'b v)                      ;=> 1   [v => #(A B)]
(vector-push 'c v)                      ;=> NIL [v => #(A B)]
~~~

### [vector-push-extend] new-element vector \[extension\] => index

~~~
(setf v (make-array 2 :fill-pointer 0 :adjustable t) ;=> #()
(vector-push 'a v)                      ;=> 0   [v => #(A)]
(vector-push 'b v)                      ;=> 1   [v => #(A B)]
(vector-push-extend 'c v)               ;=> 2   [v => #(A B C)]
~~~

### [vector-pop]

Decreases the fill pointer of vector by one, and retrieves the element of vector that is designated by the new fill pointer.

### [fill-pointer] vector => fill-pointer

Accesses the fill pointer of *vector*.

~~~
(setf arr (make-array 8 :fill-pointer 4)) ;=> #(NIL NIL NIL NIL)
(fill-pointer arr)                        ;=> 4
(vector-push 'a arr)
(fill-pointer arr)                        ;=> 5
~~~

## Bit-Vectors

* [CLtL2: Bit-Vectors](http://www.cs.cmu.edu/Groups/AI/html/cltl/clm/node161.html)

### [bit] bit-array &rest subscripts => bit

Access the bit-array element specified by *subscripts*.
[sbit] is similar but takes a [simple-bit-vector].

~~~
(setf b #*00100)
(bit b 2)          ;=> 1
(setf (bit b 2) 0) ;=> 0 [b => #\*00000]
~~~

### [bit-not] bit-arrray \[opt\] => resulting-bit-array

### [bit-eqv] bit-array1 bit-array2 \[opt\] => resulting-bit-array

### [bit-and] bit-array1 bit-array2 \[opt\] => resulting-bit-array

### [bit-andc1] bit-array1 bit-array2 \[opt\] => resulting-bit-array

### [bit-andc2] bit-array1 bit-array2 \[opt\] => resulting-bit-array

### [bit-nand] bit-array1 bit-array2 \[opt\] => resulting-bit-array

### [bit-ior] bit-array1 bit-array2 \[opt\] => resulting-bit-array

### [bit-orc1] bit-array1 bit-array2 \[opt\] => resulting-bit-array

### [bit-orc2] bit-array1 bit-array2 \[opt\] => resulting-bit-array

### [bit-xor] bit-array1 bit-array2 \[opt\] => resulting-bit-array

### [bit-nor] bit-array1 bit-array2 \[opt\] => resulting-bit-array
