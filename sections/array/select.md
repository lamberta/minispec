### [aref] array &rest subscripts => element

Access array elements.
Related: [svref] ---Access index of a simple vector.

~~~
(setf a (make-array '(2 2) :initial-contents '((1 2) (3 4))))
(aref a 1 1) ;=> 4
(setf (aref a 0 1) 99) ; a => #2A((1 99) (3 4))
~~~
