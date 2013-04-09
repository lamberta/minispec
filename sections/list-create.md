## Create

### [cons] x y => cons

~~~
(cons 1 2)      ;=> (1 . 2)
(cons 1 '(2 3)) ;=> (1 2 3)
~~~

### [list] objects\* => list

Returns a list containing *objects*.

~~~
(list 1 2 3 4)     ;=> (1 2 3 4)
~~~

### [list\*] objects\* list => list

Returns a list containing *objects* with the last argument
becoming the [cdr] of the last [cons] constructed.

~~~
(list* 'a 'b)      ;=> (A . B)
(list* 1 2 '(3 4)) ;=> (1 2 3 4)
~~~

### [make-list] size &key initial-element => list

Returns a list of length *size*.

~~~
(make-list 3)                     ;=> (NIL NIL NIL)
(make-list 2 :initial-element 'a) ;=> (A A)
~~~

### [copy-list] list => copy

Returns a copy of *list*. Only the list structure is copied;
the elements of the resulting list are the same as the
corresponding elements of the given list.
