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
