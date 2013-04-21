
* [CLtL2: Equality Predicates](http://www.cs.cmu.edu/Groups/AI/html/cltl/clm/node74.html)
* [PCL: Truth, Falsehood, and Equality](http://www.gigamonkeys.com/book/syntax-and-semantics.html#truth-falsehood-and-equality)

### [eq] x y => boolean

Returns [t] if the objects are identical. Should *not* be used
for numbers or characters since those types are
implementation-dependant.

~~~
(eq 3 3.0)                  ;=> NIL
(eq 3.0 3.0)                ;=> T
(eq "Foo" (copy-seq "Foo")) ;=> NIL
(eq "FOO" "foo")            ;=> NIL
~~~

### [eql] x y => boolean

Returns [t] if the objects are [eq] and also considers two
objects of the same class representing the same value as equivalent,

~~~
(eql 'a 'a) ;=> T
(eql 3 3)   ;=> T
(eql 3 3.0) ;=> NIL [int and float are different classes]
~~~

### [equal] x y => boolean

Considers lists equivalent if they contain the same
structure and contents. Strings are equivalent if they
contain the same characters. Falls back to [eql] for other types.

### [equalp] x y => boolean

Numbers are equivalent if they represent the same value,
ignores differences in string case. Sequences with [equalp]
elements are equivalent.

~~~
(equalp 1 1.0)           ;=> T
(equalp "HeLlo" "hello") ;=> T
~~~
