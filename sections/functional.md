# Functional Style

## Map

[HyperSpec](http://www.lispworks.com/documentation/HyperSpec/Body/f_mapc_.htm)

### map result-type fn &rest seqs... => result

~~~
(map 'list #'+ '(1 2) '(3 4)) ;=> (4 6)
(map 'vector #'(lambda (x) (* 2 x)) '(1 2 3)) ;=> #(2 4 6)
~~~

### mapcar fn &rest lists... => list

Iterates over successive list elements and returns the
accumlated results. **mapc** is similar except the results
are *not* accumulated and the first list is returned.

~~~
(mapcar #'+ '(1 2) '(3 4)) ;=> (4 6)
(mapcar (alexandria:compose #'print #'1+) '(1 2 3)) ;=> (2 3 4) [prints 2,3,4]
~~~

### mapcan fn &rest lists... => concatenated-results

### mapl fn &rest lists... => list-1

### maplist fn &rest lists... => result-list

### mapcon fn &rest lists... => concatenated-results



### alexandria:mappend fn &rest lists... => list

Zips up list components and returns a flattened list. `fn`
must return a list.

~~~
(mappend #'list '(1 3) '(2 4)) ;=> ((1 2) (3 4)) => (1 2 3 4)

(flet ((zipper (x y) (list (+ x y))))
  (mappend #'zipper '(1 3) '(2 4))) ;=> ((+ 1 2) (+ 3 4)) => (3 7)
~~~

### alexandria:map-product fn list &rest lists... => list

Results of calling `fn` with one argument per list for every combination.

~~~
(map-product #'list '(1 2) '(3 4) '(5 6))
  ;=> ((1 3 5) (1 3 6) (1 4 5) (1 4 6) (2 3 5) (2 3 6) (2 4 5) (2 4 6))
~~~

### alexandria:map-combinations fn seq &key start end length copy
### alexandria:map-derangements fn seq &key start end copy
### alexandria:map-permutations fn seq &key start end length copy

### alexandria:map-iota fn n &key start step => n

~~~
(map-iota #'print 3 :start 10 :step 2) ;=> 3 [prints 10,12,14]
~~~

## Filter

### remove-if fn seq &key from-end start end count key => seq

~~~
(remove-if #'oddp '(0 1 2 3 4)) ;=> (0 2 4)
(remove-if (alexandria:disjoin #'zerop #'oddp) '(0 1 2 3 4)) ;=> (2 4)
~~~

### remove-if-not fn seq &key from-end start end count key => seq

~~~
(remove-if-not #'oddp '(0 1 2 3 4)) ;=> (1 3)
(remove-if-not (alexandria:disjoin #'zerop #'oddp) '(0 1 2 3 4)) ;=> (0 1 3)
~~~


## Reduce

### reduce fn seq &key key from-end start end initial-value => result

~~~
(reduce #'* '(1 2 3 4 5)) ;=> 120
~~~
