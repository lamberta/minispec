### [map] result-type fn &rest seqs\* => result

Applies the function to elements of each sequence in
turn. The result sequence is as long as the shortest of the
sequences.

~~~
(map 'list #'cons '(a b) '(c d))              ;=> ((A . C) (B . D))
(map 'vector #'(lambda (x) (* 2 x)) '(1 2 3)) ;=> #(2 4 6)
~~~

### [map-into] result-seq fn &rest seqs\* => result-seq

Destructively modifies *result-seq* to contain the results
of applying the function to each element in the argument
*seqs* in turn.

~~~
(map-into '(a b c) #'oddp '(1 2 3 4 5 6)) ;=> (T NIL T)
~~~

### [remove-if-not] fn seq &key from-end start end count key => seq

Filter

~~~
(remove-if-not #'oddp '(0 1 2 3 4)) ;=> (1 3)
(remove-if-not (alexandria:disjoin #'zerop #'oddp) '(0 1 2 3 4)) ;=> (0 1 3)
~~~

### [remove-if] fn seq &key from-end start end count key => seq

~~~
(remove-if #'oddp '(0 1 2 3 4)) ;=> (0 2 4)
(remove-if (alexandria:disjoin #'zerop #'oddp) '(0 1 2 3 4)) ;=> (2 4)
~~~

### [reduce] fn seq &key key from-end start end initial-value => result

~~~
(reduce #'* '(1 2 3 4 5)) ;=> 120
~~~

### [alexandria:map-combinations] fn seq &key start end length copy
	
### [alexandria:map-derangements] fn seq &key start end copy

### [alexandria:map-permutations] fn seq &key start end length copy
