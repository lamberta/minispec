### [make-hash-table] \[:test :size :rehash-size :rehash-threshold\] => hash-table

Create and return a new [hash-table]. Default *test* is `#'eql`.

~~~
(defparameter ht (make-hash-table)) ;=> #<HASH-TABLE :TEST EQL :COUNT 0>
(setf (gethash :foo ht) 'bar)       ;=> BAR
~~~

### [alexandria:copy-hash-table] hash-table \[:key :test :size :rehash-size :rehash-threshold\] => hash-table

Returns a shallow copy of *hash-table* with the same keys, values, and properties as the original.

~~~
(setf ht2 (alexandria:copy-hash-table ht)) ;=> #<HASH-TABLE :TEST EQL :COUNT 1>
(gethash :foo ht2)                         ;=> BAR, T
~~~

### [alexandria:alist-hash-table] alist \[:test :size :rehash-size :rehash-threshold\] => hash-table

Return a [hash-table] containing the keys and values of the
association-list *alist*. Takes the same arguments as [make-hash-table].

~~~
(setf ht (alexandria:alist-hash-table '((:x . 7) (:y . 8)))) ;=> #<HASH-TABLE :TEST EQL :COUNT 2>
(gethash :x ht)                                              ;=> 7, T
~~~

### [alexandria:plist-hash-table] plist \[:test :size :rehash-size :rehash-threshold\] => hash-table

Return a [hash-table] containing the keys and values of the
property-list *plist*. Takes the same arguments as [make-hash-table].

~~~
(setf ht (alexandria:plist-hash-table '(:x 10 :y 20))) ;=> #<HASH-TABLE :TEST EQL :COUNT 2>
(gethash :y ht)                                        ;=> 20, T
~~~

### [alexandria:hash-table-alist] hash-table => list

Return an association-list containing the keys and values of *hash-table*. 

~~~
(alexandria:hash-table-alist ht) ;=> ((:Y . 20) (:X . 10))
~~~

### [alexandria:hash-table-plist] hash-table => list

Return a property-list containing the keys and values of *hash-table*. 

~~~
(alexandria:hash-table-plist ht) ;=> (:Y 8 :X 7)
~~~
