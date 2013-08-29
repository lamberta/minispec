### [hash-table-p] object => boolean

Test if *object* is of type [hash-table].

~~~
(setf ht (make-hash-table))         ;=> #<HASH-TABLE :TEST EQL :COUNT 0>
(hash-table-p ht)                   ;=> T
(hash-table-p '((:x . 7) (:y . 8))) ;=> NIL
~~~
