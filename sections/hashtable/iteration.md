### [maphash] function hash-table => nil

Iterate over *hash-table* entries, calling *function* with
two arguments, the key and value of that entry.

~~~
(setf ht (alexandria:plist-hash-table '(:x 10 :y 20))
      buf (list))
(maphash #'(lambda (key val)
             (push (list key val) buf)) ht)            ;=> NIL
buf                                                    ;=> ((:Y 20) (:X 10))
~~~

### [alexandria:maphash-keys] function hash-table => nil

Like [maphash], but calls *function* with each key in the *hash-table*. 

~~~
(setf buf (list))
(alexandria:maphash-keys #'(lambda (key)
                             (push key buf)) ht) ;=> NIL
buf                                              ;=> (:Y :X)
~~~

### [alexandria:maphash-values] function hash-table => nil

Like [maphash], but calls *function* with each value in the *hash-table*. 

~~~
(setf buf (list))
(alexandria:maphash-values #'(lambda (val)
                               (push val buf)) ht) ;=> NIL
buf                                                ;=> (20 10)
~~~

### [loop] for \[key|value\] being the \[__hash-keys__|__hash-values__\] in hash-table

Using the extended form of [loop](./loop-iterate.html#loop-extended-form),
iterate over *hash-table* entries binding to *key* and *value*.

~~~
(loop
  for key being the hash-keys in ht
  for val being the hash-values in ht
  collect (list key val))             ;=> ((:X 10) (:Y 20))
~~~

### [iterate][iterate:for...in-hashtable] for \(key value\) __in-hashtable__ hash-table

Using the [iterate](./loop-iterate.html#iterate) package,
iterate over *hash-table* entries binding to *key* and *value*.

~~~
(iter (for (key val) in-hashtable ht)
  (collect (list key val)))           ;=> ((:X 10) (:Y 20))
~~~
