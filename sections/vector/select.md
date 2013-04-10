### [svref] simple-vector index => element

Accesses the element of *simple-vector* specified by *index*.

### [fill-pointer] vector => fill-pointer

Accesses the fill pointer of *vector*.

~~~
(setf arr (make-array 8 :fill-pointer 4)) ;=> #(NIL NIL NIL NIL)
(fill-pointer arr)                        ;=> 4
(vector-push 'a arr)
(fill-pointer arr)                        ;=> 5
~~~
