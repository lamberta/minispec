### [append] &rest lists... => list

Return a new list that is the concatenation of the
arguments. The destructive version is [nconc]. To append in
place, use [alexandria:appendf].

~~~
(append '(a b) '() '(c d)) ;=> (A B C D)
~~~

### [revappend]

Also [nreconc]


### [push] item list => new-place-value

Prepend item to the front of the list and store in
place. [pushnew] will only add the item if it does not
already exist in the list.

~~~
(setf lst '(a b c))
(cons 'd lst)       ;=> (D A B C) [lst => (A B C)]
(push 'd lst)       ;=> (D A B C) [lst => (D A B C)]
~~~

### [pop] list => element

Returns the [car] of a list and store in place.

~~~
(setf lst '(a b c))
(car lst)           ;=> A [lst => (A B C)]
(pop lst)           ;=> A [lst => (B C)]
~~~

### [rplaca] cons object => cons

Replaces the [car] of the *cons* with *object*. Modifies
*cons* in-place.

~~~
(setf lst '(a b c)) 
(rplaca lst 'd)     ;=> (D B C) 
~~~

### [rplacd] cons object => cons

Replaces the [cdr] of the *cons* with *object*. Modifies
*cons* in-place.

~~~
(setf lst '(a b c))
(rplacd lst '(d e)) ;=> (A D E)
~~~
