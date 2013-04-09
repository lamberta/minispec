### [length] seq => n

~~~
(length '(a b c)) ;=> 3
(length "hello") ;=> 5
~~~

### [elt] seq idx => element

Use `alexandria:random-elt` to return a random element.
Related: `nth`, `nthcdr`, `car`, `cdr`, `first` ... `tenth`

~~~
(setf seq '(a b c))
(elt seq 1)             ;=> B
(setf (elt seq 1) "hi") ; seq => (A "hi" C)
~~~

### [subseq] seq start &optional end => sub-seq

Returns the subsequence of *seq* specified by *start* and *end*.

~~~
(setf str "hello")
(subseq str 2 4) ;=> "ll"
(setf (subseq str 2 4) "ad") ; str => "heado"
~~~

### [cl-utilities:extremum] seq fn &key key (start 0) end => smallest-element

Returns first element of *sequence* if it were ordered by
`sort` using the predicate *fn*. `extrema` is similar but
returns a list of values since there may be more than one
extremum determined by the predicate. `n-most-extreme`
returns a list of *n* values of a sorted sequence. [ref](http://common-lisp.net/project/cl-utilities/doc/extremum.html)

~~~
(extremum '(1 2 9 7 3 2) #'>)         ;=> 9
(extrema '(1 2 9 7 3 2) #'>)          ;=> (9)
(n-most-extreme 3 '(1 2 9 7 3 2) #'>) ;=> (9 7 3)
~~~

### [find] item seq &key from-end test test-not start end key => element

If the sequence contains an element satisfying the test,
then the leftmost such element is returned; otherwise [nil]
is returned. Functional variants are [find-if] and [find-if-not].

~~~
(find 3 '(1 2 3 4 5))                            ;=> 3
(find-if #'oddp '(1 2 3 4 5) :end 3 :from-end t) ;=> 3
~~~

### [position] item seq &key from-end test test-not start end key => idx

Return the first index position of an item in the sequence,
otherwise [nil].

~~~
(position #\a "baobab" :from-end t)                         ;=> 4
(position-if #'oddp '((1) (2) (3) (4)) :start 1 :key #'car) ;=> 2
~~~

### [count] item seq &key from-end start end key test test-not => n

The number of elements in the specified subsequence of
*seq*. Functional variants are [count-if] and [count-if-not].

~~~
(count 'a '(a b c a))      ;=> 2
(count-if #'oddp '(1 2 3)) ;=> 2
~~~

### [search] seq1 seq2 &key from-end test test-not key start1 start2 end1 end2 => idx

Searches sequence *seq2* for a subsequence that matches
*seq1*. Returns its index position.

~~~
(search '(c d) '(a b c d)) ;=> 2
(search "bar" "foobarbaz") ;=> 3
~~~

### [mismatch] seq1 seq2 &key from-end test test-not key start1 start2 end1 end2 => idx

Return the index position where two sequences diverge.

~~~
(mismatch "foobarbaz" "foom") ;=> 3
~~~
