## Lists

* [PCL: List Processing](http://www.gigamonkeys.com/book/they-called-it-lisp-for-a-reason-list-processing.html)

### [copy-list]

### [append] &rest lists... => list

Return a new list that is the concatenation of the
arguments. The destructive version is `nconc`. To append in
place, use `alexandria:appendf`.

~~~
(append '(a b) '() '(c d)) ;=> (A B C D)
~~~

### [alexandria:mappend] fn &rest lists... => list

Zips up list components and returns a flattened list. *fn*
must return a list.

~~~
(mappend #'list '(1 3) '(2 4)) ;=> ((1 2) (3 4)) => (1 2 3 4)

(flet ((zipper (x y) (list (+ x y))))
  (mappend #'zipper '(1 3) '(2 4))) ;=> ((+ 1 2) (+ 3 4)) => (3 7)
~~~

### [push] item list => new-place-value

Prepend item to the front of the list and store in
place. `pushnew` will only add the item if it does not
already exist in the list.

~~~
(setf lst '(a b c))
(cons 'd lst)       ;=> (D A B C) [lst => (A B C)]
(push 'd lst)       ;=> (D A B C) [lst => (D A B C)]
~~~

### [pop] list => element

Returns the `car` of a list and store in place.

~~~
(setf lst '(a b c))
(car lst)           ;=> A [lst => (A B C)]
(pop lst)           ;=> A [lst => (B C)]
~~~

### [butlast] list &optional n => result-list

Returns a copy of *list* with the last *n* conses omitted, defaults to 1.
The destructive vesion is `nbutlast`.

~~~
(butlast '(a b c d))   ;=> (A B C)
(butlast '(a b c d) 2) ;=> (A B)
~~~
