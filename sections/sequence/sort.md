## Sort

### [reverse] seq => reversed-seq

Reverse the order of elements in a sequence. The destructive
version is [nreverse]. To save the result in place, use [alexandria:reversef].

~~~
(reverse '(a b c d)) ;=> (D C B A)
~~~

### [sort] seq fn &key key => sorted-seq

The sequence is destructively sorted according to an order
determined by the predicate *fn*. [stable-sort] guarantees
equal elements stay in same order.

~~~
(sort '(3 1 4 2) (lambda (x y) (< x y))) ;=> (1 2 3 4)
~~~

### [merge] result-type seq1 seq2 fn &key key => result-seq

Destructively concatenates the two sequences and sorts the
combined elements based on the predicate *fn*.

~~~
(merge 'list '(1 3 5) '(2 4 6) #'<) ;=> (1 2 3 4 5 6)
~~~

### [alexandria:rotate] seq &optional n => result-seq

Returns a *sequence* with elements rotated by *n*, defaulting to 1.

~~~
(rotate '(a b c))    ;=> (C A B)
(rotate '(a b c) -1) ;=> (B C A)
~~~

### [alexandria:shuffle] seq &key start end => result-seq

Returns a random permutation of a sequence bounded by *:start*
and *:end*. The original sequence may be modified.
