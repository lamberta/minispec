## Sequences

Slicing and dicing. The type sequence encompasses both lists
and vectors (one-dimensional arrays).

* [HyperSpec: Sequences Dictionary](http://www.lispworks.com/documentation/HyperSpec/Body/c_sequen.htm)
* [PCL: Ch 11. Collections](http://www.gigamonkeys.com/book/collections.html)
* [CLtL2: Ch 14. Sequences](http://www.cs.cmu.edu/Groups/AI/html/cltl/clm/node141.html)


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

### [copy-seq] seq => copied-seq

### [length] seq => n

~~~
(length '(a b c)) ;=> 3
(length "hello") ;=> 5
~~~

### [make-sequence] result-type size &key initial-element => seq

~~~
(make-sequence '(vector double-float) 100 :initial-element 1d0)
~~~

## Concatenating, Mapping, and Reducing Sequences

### [concatenate] result-type &rest seqs... => result-seq

~~~
(concatenate 'list '(a b) '(c d)) ;=> (A B C D)
(concatenate 'string "hello" "world") ;=> "helloworld"
~~~

### [every] fn seq &rest seqs... => boolean

### [some] fn seq &rest seqs... => obj

### [notevery] fn seq &rest seqs... => boolean

### [notany] fn seq &rest seqs... => boolean

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

### [cl-utilities:split-sequence] delimiter seq &key count remove-empty-subseqs from-end start end test test-not key => list, idx

Splits sequence into a *list* of subsequences delimited by
objects satisfying the test. Also returns the lenbth of the
sequence *idx*. Functional variants are `split-sequence-if`
and `split-sequence-if-not`. [ref](http://common-lisp.net/project/cl-utilities/doc/split-sequence.html)

~~~
(split-sequence #\Space "hello world")           ;=> ("hello" "world"), 11
(split-sequence-if #'evenp '(1 1 2 1 3 4 1 3 5)) ;=> ((1 1) (1 3) (1 3 5)), 9
~~~

## Modify

### [fill] seq item &key start end => seq

### [replace] seq1 seq2 &key start1 end1 start2 end2 => seq1

~~~
(replace "abcde" "01234" :start1 1 :end1 3 :start2 3) ;=> "a34de"
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

Destrictive variant is [delete-duplicates].

~~~
(remove-duplicates '(1 2 1 2 3 1 2 3 4)) ;=> (1 2 3 4)
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

## Search

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


## Sort and Merge

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
