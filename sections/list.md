## Access


### [list-length]

### [first]

Also [car]

~~~
(first '(:a :b :c)) ;=> :A
(car '(:a :b :c))   ;=> :A
~~~

### [rest]

Also [cdr]

~~~
(rest '(:a :b :c)) ;=> (:B :C)
(cdr '(:a :b :c))  ;=> (:B :C)
~~~

cXr ... cadr, caddr, etc.

### [nth]

Also [first], [second], [third], ... [tenth].

### [nthcdr]

~~~
(nthcdr 2 '(:a :b :c :d)) ;=> (:C :D)
~~~

### [last]

~~~
(last '(:a :b :c :d))   ;=> (:D)
(last '(:a :b :c :d) 2) ;=> (:C :D)
~~~

### [butlast] list &optional n => result-list

Returns a copy of *list* with the last *n* conses omitted, defaults to 1.
The destructive vesion is [nbutlast].

~~~
(butlast '(a b c d))   ;=> (A B C)
(butlast '(a b c d) 2) ;=> (A B)
~~~


### [append] &rest lists... => list

Return a new list that is the concatenation of the
arguments. The destructive version is [nconc]. To append in
place, use [alexandria:appendf].

~~~
(append '(a b) '() '(c d)) ;=> (A B C D)
~~~

### [revappend]

Also [nreconc]

### [ldiff] list sublist => result-list

Returns the list difference, in a new list, whose elements
of *list* appear before *sublist*. If *sublist* is not the
tail of *list*, then return a copy of the entire *list*.

~~~
(setf list1 '(a b c d))
(setf list2 (last list1 2)) ;=> (C D)
(ldiff list1 list2)         ;=> (A B)
~~~


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
