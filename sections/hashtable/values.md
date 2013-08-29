### [hash-table-count] hash-table => n

Return the number of entries in *hash-table*.

~~~
(setf ht (make-hash-table))   ;=> #<HASH-TABLE :TEST EQL :COUNT 0>
(hash-table-count ht)         ;=> 0
(setf (gethash :foo ht) 'bar)
(hash-table-count ht)         ;=> 1
~~~

### [gethash] key hash-table \[default\] => value, present-p

Return a *key*'s entry value in *hash-table*, or *default*
if none. If entry is found, *present-p* is [t]. Store values with [setf].

~~~
(gethash :foo ht)             ;=> NIL, NIL
(gethash :foo ht 'default)    ;=> DEFAULT, NIL
(setf (gethash :foo ht) 'bar)
(gethash :foo ht)             ;=> BAR, T
~~~

### [alexandria:ensure-gethash] key hash-table \[default\] => value, present-p

Like [gethash], but if *key* is not found in *hash-table*,
save *default* as an entry before returning it.

~~~
(setf ht (make-hash-table))          ;=> #<HASH-TABLE :TEST EQL :COUNT 0>
(alexandria:ensure-gethash :z ht 99) ;=> 99, NIL
(gethash :z ht)                      ;=> 99, T
~~~

### [remhash] key hash-table => present-p

Removes entry for *key* in *hash-table*. Returns [t] if
there was such an entry, or [nil] otherwise.

~~~
(setf ht (alexandria:plist-hash-table '(:x 10 :y 20)))
(remhash :x ht)                                        ;=> T
(remhash :z ht)                                        ;=> NIL
~~~

### [clrhash] hash-table => hash-table

Removes all entries from *hash-table*, and then returns that empty *hash-table*.

~~~
(setf ht (alexandria:plist-hash-table '(:x 10 :y 20)))
(hash-table-count ht)                                  ;=> 2
(clrhash ht)                                           ;=> #<HASH-TABLE :TEST EQL :COUNT 0>
(hash-table-count ht)                                  ;=> 0
~~~

### [alexandria:hash-table-keys] hash-table => list

Returns a [list] containing the keys of *hash-table*.

~~~
(setf ht (alexandria:plist-hash-table '(:x 10 :y 20)))
(alexandria:hash-table-keys ht)                        ;=> (:Y :X)
~~~

### [alexandria:hash-table-values] hash-table => list

Returns a list containing the values of *hash-table*.

~~~
(setf ht (alexandria:plist-hash-table '(:x 10 :y 20)))
(alexandria:hash-table-values ht)                      ;=> (20 10)
~~~
