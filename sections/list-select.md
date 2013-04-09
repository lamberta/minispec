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

### [ldiff] list sublist => result-list

Returns the list difference, in a new list, whose elements
of *list* appear before *sublist*. If *sublist* is not the
tail of *list*, then return a copy of the entire *list*.

~~~
(setf list1 '(a b c d))
(setf list2 (last list1 2)) ;=> (C D)
(ldiff list1 list2)         ;=> (A B)
~~~
