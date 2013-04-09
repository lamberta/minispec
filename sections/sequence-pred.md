### [some] fn seq &rest seqs\* => result

Returns the first non-[nil] value which is returned by an
invocation of the predicate *fn*.

~~~
(some #'evenp '(1 2 3 4))  ;=> T
(some #'1+ '(10 20 30 40)) ;=> 11
(some #'numberp '(a b c))  ;=> NIL
~~~

### [every] fn seq &rest seqs\* => boolean

Returns [nil] as soon as any invocation of the predicate
*fn* returns [nil].

~~~
(every #'numberp '(1 2 3 4)) ;=> T
(every #'evenp '(1 2 3 4))   ;=> NIL
~~~

### [notevery] fn seq &rest seqs\* => boolean

Returns [t] as soon as any invocation of predicate *fn*
returns [nil]. 

~~~
(notevery #'numberp '(1 2 3 4)) ;=> NIL
(notevery #'evenp '(1 2 3 4))   ;=> T
~~~

### [notany] fn seq &rest seqs\* => boolean

Returns [nil] as soon as any invocation of predicate *fn*
returns [t].

~~~
(notany #'numberp '(1 2 3 4)) ;=> NIL
(notany #'evenp '(1 2 3 4))   ;=> NIL
~~~
