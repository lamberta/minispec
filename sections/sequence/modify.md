### [concatenate] result-type &rest seqs... => result-seq

~~~
(concatenate 'list '(a b) '(c d)) ;=> (A B C D)
(concatenate 'string "hello" "world") ;=> "helloworld"
~~~

### [cl-utilities:split-sequence] delimiter seq &key count remove-empty-subseqs from-end start end test test-not key => list, idx

Splits sequence into a *list* of subsequences delimited by
objects satisfying the test. Also returns the lenbth of the
sequence *idx*. Functional variants are
[cl-utilities:split-sequence-if] and [cl-utilities:split-sequence-if-not].

~~~
(split-sequence #\Space "hello world")           ;=> ("hello" "world"), 11
(split-sequence-if #'evenp '(1 1 2 1 3 4 1 3 5)) ;=> ((1 1) (1 3) (1 3 5)), 9
~~~

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
