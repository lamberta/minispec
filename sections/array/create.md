### [make-array] dims &key type initial-element initial-contents adjustable fill-pointer displaced-to displaced-index-offset => new-array

~~~
(make-array '(2 3)) ;=> #2A((0 0 0) (0 0 0))
(make-array 5 :fill-pointer 0 :adjustable t :element-type 'character) ;=> ""
~~~

### [adjust-array] array new-dims &key type initial-element initial-contents fill-pointer displaced-to displaced-index-offset => adjusted-array

### [alexandria:copy-array] array &key element-type fill-pointer adjustable => new-array

Returns an undisplaced copy of *array*, with same
fill-pointer and adjustability as the original.
