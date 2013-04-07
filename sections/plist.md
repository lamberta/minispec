## Property Lists (plist)

Lists with alternating keys and values: `(:x 10 :y 20)`.
Uses `#'eq` for `:test`, so should only use symbols as
keywords.

Every symbol contains metadata on it stored as a plist:

~~~
(symbol-plist 'symbol) ;access plist of symbol
(get 'symbol 'key) ;equivalent to (getf (symbol-plist 'symbol)
(setf (get 'symbol :my-key) "information") ;set value
(remprop 'symbol 'my-key) ;remove property, same as (remf (symbol-plist 'symbol) 'my-key)
~~~

### getf plist indicator &optional default => value

~~~
(getf plist :x)           ;=> 10
(setf (getf plist :x) 30) ;=> 30
~~~

### remf place indicator => boolean

~~~
(remf plist :x) ;=> T
~~~

### get-properties plist indicator-list => indicator, value, tail

Used since `getf` can't distinguish an absent property from
*nil*. Returns on the first key found.

~~~
(get-properties plist '(:x :y)) ;=> :X, 10, (:X 10 :Y 20)
~~~

### alexandria:plist-alist plist

### alexandria:plist-hash-table plist &rest hash-table-initargs

### alexandria:remove-from-plist plist &rest keys

### alexandria:remove-from-plistf plist &rest keys

destructive
