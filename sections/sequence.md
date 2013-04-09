## Sequences

Slicing and dicing. The type sequence encompasses both lists
and vectors (one-dimensional arrays).

* [HyperSpec: Sequences Dictionary](http://www.lispworks.com/documentation/HyperSpec/Body/c_sequen.htm)
* [PCL: Ch 11. Collections](http://www.gigamonkeys.com/book/collections.html)
* [CLtL2: Ch 14. Sequences](http://www.cs.cmu.edu/Groups/AI/html/cltl/clm/node141.html)


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

### [concatenate] result-type &rest seqs... => result-seq

~~~
(concatenate 'list '(a b) '(c d)) ;=> (A B C D)
(concatenate 'string "hello" "world") ;=> "helloworld"
~~~

### [subseq] seq start &optional end => sub-seq

Returns the subsequence of *seq* specified by *start* and *end*.

~~~
(setf str "hello")
(subseq str 2 4) ;=> "ll"
(setf (subseq str 2 4) "ad") ; str => "heado"
~~~

### [cl-utilities:split-sequence] delimiter seq &key count remove-empty-subseqs from-end start end test test-not key => list, idx

Splits sequence into a *list* of subsequences delimited by
objects satisfying the test. Also returns the lenbth of the
sequence *idx*. Functional variants are `split-sequence-if`
and `split-sequence-if-not`. [ref](http://common-lisp.net/project/cl-utilities/doc/split-sequence.html)

~~~
(split-sequence #\Space "hello world")           ;=> ("hello" "world"), 11
(split-sequence-if #'evenp '(1 1 2 1 3 4 1 3 5)) ;=> ((1 1) (1 3) (1 3 5)), 9
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


## Modify

### [fill] seq item &key start end => seq

Destructively replaces the elements of *seq* bounded by
*:start* and *:end* with *item*.

~~~
(fill '(a b c d) 'x :start 1 :end 3) ;=> (A X X D)
~~~

### [replace] seq1 seq2 &key start1 end1 start2 end2 => seq1

Destructively replaces the elements of *se1* bounded by
*:start1* and *:end1* with the elements of *seq2* bounded by
*:start2* and *:end2*.

~~~
(replace "abcde" "98765" :start1 1 :end1 3 :start2 3) ;=> "a65de"
~~~

### [substitute] new old seq &key from-end test test-not start end count key

Functional variants are [substitute-if],
[substitute-if-not]. Destructive variants are [nsubstitute],
[nsubstitute-if], and [nsubstitute-if-not]. [subst] performs
substitutions throughout a *tree*.

~~~
(substitute 10 1 '(1 2 1 3 1 4))      ;=> (10 2 10 3 10 4)
(substitute-if 0 #'oddp '(1 2 3 4 5)) ;=> (0 2 0 4 0)
~~~

### [remove] item seq &key from-end test test-not start end count key => result-seq

Functional variants are [remove-if] and [remove-if-not]. The
destructive variants are [delete], [delete-if], and
[delete-if-not]. To remove in place, use
[alexandria:removef] or [alexandria:deletef].

~~~
(remove 4 '(1 2 4 1 3 4 5))           ;=> (1 2 1 3 5)
(remove-if #'oddp '(1 2 4 1 3 4 5))   ;=> (2 4 4)
(remove-if-not #'oddp '(1 2 1 3 4 5)) ;=> (1 1 3 5)
~~~

### [remove-duplicates] seq &key from-end test test-not start end key

Destructive variant is [delete-duplicates].

~~~
(remove-duplicates '(1 2 1 2 3 1 2 3 4)) ;=> (1 2 3 4)
~~~
