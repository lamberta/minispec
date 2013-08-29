### [with-hash-table-iterator] \(iter hash-table) form\* => result\*

Generates an iterator [function] *iter* to successively step
through entries in a *hash-table*. The *iter* function returns
three values: a [boolean] if an entry was returned, the key
from the *hash-table* entry, and the value from the
*hash-table* entry.

~~~
(setf ht (alexandria:plist-hash-table '(:x 10 :y 20))
      buf (list))
(with-hash-table-iterator (next-entry ht)
  (loop
    (multiple-value-bind (entry-p key val) (next-entry)
      (if entry-p
        (push (list key val) buf)
        (return)))))                                     ;=> NIL
buf                                                      ;=> ((:Y 20) (:X 10))
~~~

### [hash-table-test] hash-table => test

Return the equality test used for comparing keys in *hash-table*.

~~~
(setf ht (make-hash-table))      ;=> #<HASH-TABLE :TEST EQL :COUNT 0>
(setf test (hash-table-test ht)) ;=> EQL
(funcall test 6 6)               ;=> T
~~~

### [hash-table-size] hash-table => size

Return the current size of *hash-table*, an
implementation-dependent value used to determine the amount
of entries a [hash-table] can hold before it has to grow.

~~~
(setf ht (make-hash-table)) ;=> #<HASH-TABLE :TEST EQL :COUNT 0>
(hash-table-size ht)        ;=> 16 (implementation-dependent)
~~~

### [hash-table-rehash-size] hash-table => rehash-size

Return the current rehash size of *hash-table*, an
implementation-dependent value used to determine the minimum
amount to increase the size of the [hash-table] when it
becomes full enough to require rehashing.

~~~
(setf ht (make-hash-table)) ;=> #<HASH-TABLE :TEST EQL :COUNT 0>
(hash-table-rehash-size ht) ;=> 1.5 (implementation-dependent)
~~~

### [hash-table-rehash-threshold] hash-table => rehash-threshold

Return the current rehash threshold of *hash-table*, an
implementation-dependent value used to determine how full
the [hash-table] can get before it must grow.

~~~
(setf ht (make-hash-table))      ;=> #<HASH-TABLE :TEST EQL :COUNT 0>
(hash-table-rehash-threshold ht) ;=> 1.0 (implementation-dependent)
~~~

### [sxhash] object => hash-code

Returns a hash-code for the *object*. The manner in which it
is computed is implementation-dependent.

~~~
(eql (sxhash 'a) (sxhash 'a)) ;=> T
(eql (sxhash 'a) (sxhash 'b)) ;=> NIL
~~~
