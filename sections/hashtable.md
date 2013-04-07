## Hash-Tables

* [PCL](http://gigamonkeys.com/book/collections.html#hash-tables)
* [HS](http://www.lispworks.com/documentation/HyperSpec/Body/18_aa.htm)

### [make-hash-table] &key test size rehash-size rehash-threshold => hash-table

Default *test* is `#'eql`.

### [alexandria:alist-hash-table] alist &rest hash-table-initargs => hash-table

Converts an *alist* to a *hash-table* format.

### [alexandria:plist-hash-table] plist &rest hash-table-initargs => hash-table

### [hash-table-p] object => boolean

Test if *object* is of type [hash-table].

### [hash-table-count] hash-table => n

Number of entries in *hash-table*.

### [gethash] key hash-table &optional default => value, present-p

Return the entry value from a *hash-table* given its
key. Store values with [setf].

~~~
(gethash :foo ht)             ;=> NIL, NIL
(setf (gethash :foo ht) 'bar) ;=> BAR
(gethash :foo ht)             ;=> BAR, T
~~~

### [alexandria:ensure-gethash] key hash-table &optional default => value, present-p

Like `gethash`, but if key is not found in the *hash-table*
saves the *default* under key before returning it.

### [remhash] key hash-table => boolean

Removes entry for *key*, if any. Returns *true* if there,
*false* otherwise.

### [clrhash] hash-table => hash-table

Removes all entries from *hash-table*, and then returns that empty *hash-table*.

### [alexandria:hash-table-keys] hash-table => list

### [alexandria:hash-table-values] hash-table => list

### [alexandria:copy-hash-table] table &key key test size rehash-size rehash-threshold => hash-table

### [alexandria:hash-table-alist] hash-table => list

### [alexandria:hash-table-plist] hash-table => list
