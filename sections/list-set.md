## Sets

* [PCL: Beyond Lists: Other Uses for Cons Cells](http://www.gigamonkeys.com/book/beyond-lists-other-uses-for-cons-cells.html#sets)

### [alexandria:setp] object &key test key => boolean

Test if *object* is a [list] containing unique elements.

~~~
(setp '(a b c)) ;=> T
(setp '(a b b)) ;=> NIL
(setp '((10 :a) (10.0 :b)) :key #'car)           ;=> T
(setp '((10 :a) (10.0 :b)) :key #'car :test #'=) ;=> NIL
~~~

### [subsetp] list1 list2 &key key test test-not => boolean

Test if every element of *list1* matches an element in *list2*.

~~~
(subsetp '(c b a) '(a b c d)) ;=> T
(subsetp '(a b c d) '(c b a)) ;=> NIL
~~~

### [alexandria:set-equal] list1 list2 &key test key => boolean

Test if every element in *list1* matches an element in
*list2*, and vice-versa.

~~~
(set-equal '(a b c) '(c b a)) ;=> T
(set-equal '(a b) '(a b c))   ;=> NIL
~~~

### [find] item seq &key from-end test test-not start end key => element

Search for an element in the sequence, return the found
element or [nil]. Functional variants are [find-if] and [find-if-not].

### [member] item list &key key test test-not => tail

Similiar to [find], but if the item is in the list return
the tail, otherwise [nil]. Functional variants are
[member-if] and [member-if-not].

~~~
(member 'c '(a b c d))             ;=> (C D)
(member-if #'evenp '(1 2 3 4))     ;=> (2 3 4)
(member-if-not #'evenp '(1 2 3 4)) ;=> (1 2 3 4)
~~~

### [adjoin] item list &key key test test-not => new-list

Add item to *list* if not already there, otherwise return
original list. Use [pushnew] to modify the original list.

### [union] list1 list2 &key key test test-not => result-list

To save the result in place, use [alexandria:unionf].

~~~
(union '(a b c) '(f a d)) ;=> (A B C D F)
~~~

### [intersection] list1 list2 &key key test test-not => result-list

Return a list that contains every element in both *list1* and *list2*.

~~~
(intersection '(a b c) '(b c d)) ;=> (B C)
~~~

### [set-difference] list1 list2 &key key test test-not => result-list

Returns a list of elements of *list1* that do not appear in *list2*.

~~~
(set-difference '(a b c e) '(b c d)) ;=> (A E)
~~~

### [set-exclusive-or] list1 list2 &key key test test-not => result-list

Returns a list of elements that appear in exactly one of *list1* and *list2*.

~~~
(set-exclusive-or '(a b c e) '(b c d)) ;=> (A D E)
~~~
