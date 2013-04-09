### [dolist] (var list \[result\]) form\* => result\*

Iterate over the elements of a [list]. Terminate loop immediately with [return].

~~~
(dolist (x '(1 2 3 4))
  (print x))

(dolist (x '(1 2 3 4) (print 'done))
  (print x))
~~~

### [mapcar]

Iterates over successive list elements and returns the
accumlated results. **mapc** is similar except the results
are *not* accumulated and the first list is returned.

~~~
(mapcar #'+ '(1 2) '(3 4)) ;=> (4 6)
(mapcar (alexandria:compose #'print #'1+) '(1 2 3)) ;=> (2 3 4) [prints 2,3,4]
~~~

### [maplist]

### [mapcan]

### [mapcon]

### [mapc]

### [mapl]

### [alexandria:mappend] fn &rest lists... => list

Zips up list components and returns a flattened list. *fn*
must return a list.

~~~
(mappend #'list '(1 3) '(2 4)) ;=> ((1 2) (3 4)) => (1 2 3 4)

(flet ((zipper (x y) (list (+ x y))))
  (mappend #'zipper '(1 3) '(2 4))) ;=> ((+ 1 2) (+ 3 4)) => (3 7)
~~~

### [alexandria:map-product] fn list &rest lists... => list

Results of calling `fn` with one argument per list for every combination.

~~~
(map-product #'list '(1 2) '(3 4) '(5 6))
  ;=> ((1 3 5) (1 3 6) (1 4 5) (1 4 6) (2 3 5) (2 3 6) (2 4 5) (2 4 6))
~~~
